pub use errr_proc::*;

// Sum Type ////////////////////////////////////////////////////////////////////////////////////////

/// A Sum-Type like [`Result`], but with different names.
///
/// We can encode an arbitrary enum as a nested [`enum@Sum`], e.g.
/// ```
/// enum SomeError {
///     ErrorA(u64),
///     ErrorB(String),
///     ErrorC(f32),
/// }
/// ```
/// can be encoded as
/// ```
/// type SumError = Sum<u64, Sum<String, Sum<f32, Void>>>;
/// ```
/// The constructor names reflect that we encode enums always as a list and never as a real tree,
/// i.e. if we nest a [`enum@Sum`] inside a [`enum@Sum`] it always fills the right parameter `B`
/// and never the left parameter `A`.
/// For example, if we construct a value of type `SumError`, then the number of `There` constructors
/// is the index in the type-level list:
/// ```
/// let err_a: SumError = Here(42);                    // Index 0: u64
/// let err_b: SumError = There(Here("".to_owned()));  // Index 1: String
/// let err_c: SumError = There(There(Here(5.2)));     // Index 2: f32
/// ```
///
/// The [`Sum!`] macro gives more readable syntax to write down nested
/// [`enum@Sum`]-Types:
/// ```
/// type SumError = Sum!(u64, String, f32);
/// ```
#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub enum Sum<A, B> {
    Here(A),
    There(B),
}

/// A type with no elements.
///
/// This acts as a neutral element to [`enum@Sum`]:
/// The type [`enum@Sum`]`<A, `[`Void`]`>` is isomorphic to `A`, i.e. while both types have different
/// internal structure, they have the same number of elements with the same meaning and behave the
/// same for all things we're interested in.
#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub enum Void {}

pub use Sum::{Here, There};

// Macros //////////////////////////////////////////////////////////////////////////////////////////

/// Nicer syntax to write down nested [`enum@Sum`]-types.
///
/// The macro invocation
/// ```
/// Sum!(A, B, C)
/// ```
/// expands to
/// ```
/// Sum<A, Sum<B, Sum<C, Void>>>
/// ```
#[macro_export]
macro_rules! Sum {
    ($t:ty , $($ts:tt)*) => {Sum<$t, Sum!($($ts)*)>};
    ($t:ty) => {Sum<$t, Void>};
    () => {Void};
}

/// Nicer syntax to match on values of nested [`enum@Sum`] types.
///
/// The macro invocation
/// ```
/// let e: Sum!(u64, f32, bool);
/// match_sum_inner!(e => {
///     42 => {},
///     5.2 => {},
///     true => {},
/// });
/// ```
/// expands to
/// ```
/// let e: Sum!(u64, f32, bool);
/// match e {
///     Here(42) => {},
///     There(Here(5.2)) => {},
///     There(There(Here(true))) => {},
/// };
/// ```
#[macro_export]
macro_rules! match_sum_inner {
    ( ($e:expr) { $pat:pat => { $($body:tt)* } $($rest:tt)* } ) => {
        match matches($e) {
            Ok($pat) => { $($body)* }
            Err(rest) => { match_sum_inner!((rest) { $($rest)* }) }
        }
    };
    ( ($e:expr) { } ) => {
        match $e {}
    }
}

/// Nicer syntax to match on values of types, which implement `Has<T1, N1> + ... + Has<Tn, Nn>`,
/// by first converting them to `Sum!(T1, ..., Tn)`.
///
/// This is similar to [`match_sum_inner!`] but converts the polymorphic variant to a [`Sum!`] based
/// on the patterns of the match arms:
///
/// The macro invocation
/// ```
/// match_sum!(e => {
///     ErrA(a) => {},
///     ErrB(b) => {},
///     ErrC(c) => {},
/// });
/// ```
/// expands to
/// ```
/// let e: Sum!(ErrA, ErrB, ErrC) = e;
/// match e {
///     Here(ErrA(a)) => {},
///     There(Here(ErrB(b))) => {},
///     There(There(Here(ErrC(c)))) => {},
/// };
/// ```
#[macro_export]
macro_rules! match_sum {
    ( $e:expr => { $( $err:ident($pat:pat) => { $($body:tt)* } )* } ) => {{
        let e: Sum!($($err),*) = $e;
        match_sum_inner!((e) { $( $err($pat) => { $($body)* } )* })
    }}
}

/// Nicer syntax to match on a [`Result<T, E>`] value where `E` is a polymorphic variant.
#[macro_export]
macro_rules! match_sum_res {
    ( $e:expr => { Ok($p:pat) => { $($body:tt)* } $($rest:tt)* } ) => {{
        match $e {
            Ok($p) => { $($body)* }
            Err(e) => match_sum!(e => { $($rest)* })
        }
    }}
}

// Natural Numbers at the Type Level ///////////////////////////////////////////////////////////////

/// The kind of types which represent natural numbers.
///
/// Example: the number `2` is represented by the type `Succ<Succ<Zero>>`.
pub trait Nat {}

/// The type `Zero` represents the natural number `0` at the type level.
pub struct Zero {}

/// The type `Succ<N>` represents the natural number `N + 1` at the type level.
pub struct Succ<N: Nat> {
    phantom: std::marker::PhantomData<N>,
}

impl Nat for Zero {}
impl<N: Nat> Nat for Succ<N> {}

/// The kind of types which represent lists of natural numbers.
///
/// Example: the list `[0, 1, 0]` is represented by the type
/// `Cons<Zero, Cons<Succ<Zero>, Cons<Zero, Nil>>>`.
pub trait Nats {}

/// The type `Nil` represents the empty list at the type level.
pub struct Nil {}

/// The type `Cons<N, Ns>` represents a list with head `N` and tail `Ns` at the
/// type level.
pub struct Cons<N: Nat, Ns: Nats> {
    phantom_n: std::marker::PhantomData<N>,
    phantom_ns: std::marker::PhantomData<Ns>,
}

impl Nats for Nil {}
impl<Ns: Nats, N: Nat> Nats for Cons<N, Ns> {}

// Has /////////////////////////////////////////////////////////////////////////////////////////////

/// `Has<T, N>` describes types which behave like an enum whose `N`-th constructor contains
/// type `T`.
/// If there is only one constructor of type `T`, then the compiler can infer the `N`.
pub trait Has<T, N: Nat> {
    /// A type which has all the constructors of `Self` except the one for `T`.
    type WithoutT;
    /// Inject a `T` into `Self` by using its constructor.
    fn inject(t: T) -> Self;
    /// Match on `Self` and return either a `T` or a value of an enum which
    /// could be any other constructor of `Self` except for `T`.
    fn pull_out(self) -> Sum<T, Self::WithoutT>;
}

/// Inject a `T` into `Ts` by using its constructor.
pub fn inject<T, Ts: Has<T, impl Nat>>(t: T) -> Ts {
    Ts::inject(t)
}

/// Match on `Self` and return either a `T` or a value of an enum which could be any other
/// constructor of `Self` except for `T`.
pub fn matches<T, Ts: Has<T, impl Nat>>(ts: Ts) -> Result<T, Ts::WithoutT> {
    match ts.pull_out() {
        Here(t) => Ok(t),
        There(ts) => Err(ts),
    }
}

/// If `T` is the first constructor of `Self`.
impl<T, Ts> Has<T, Zero> for Sum<T, Ts> {
    type WithoutT = Ts;
    fn inject(t: T) -> Self {
        Here(t)
    }
    fn pull_out(self) -> Sum<T, Self::WithoutT> {
        self
    }
}

/// If `T` is not the first constructor of `Self`.
impl<T, U, Ts: Has<T, N>, N: Nat> Has<T, Succ<N>> for Sum<U, Ts> {
    type WithoutT = Sum<U, Ts::WithoutT>;
    fn inject(t: T) -> Self {
        There(Ts::inject(t))
    }

    fn pull_out(self) -> Sum<T, Self::WithoutT> {
        match self {
            Here(u) => There(Here(u)),
            There(ts) => match ts.pull_out() {
                Here(t) => Here(t),
                There(ts) => There(There(ts)),
            },
        }
    }
}

// EmbedIn /////////////////////////////////////////////////////////////////////////////////////////

/// If we have some enum `Self`, which has a subset of the constructors of some enum `Ts`, then we
/// can convert `Self` to `Ts`.
///
/// `Ns` is a list of type level natural numbers, which describes for each constructor from `Self`
/// at which index the corresponding constructor in `Ts` is.
///
/// `Ns` is inferred by the compiler, if the constructor types are unique.
pub trait EmbedIn<Ts, Ns: Nats> {
    /// Embed the constructors from `Self` into `Ts`.
    fn embed(self) -> Ts;
}

/// Recursive case: Try to embed the first constructor from `Self` otherwise recursively embed the
/// rest.
impl<T, N, Ns, Ts, Us> EmbedIn<Us, Cons<N, Ns>> for Sum<T, Ts>
where
    N: Nat,
    Ns: Nats,
    Ts: EmbedIn<Us, Ns>,
    Us: Has<T, N>,
{
    fn embed(self) -> Us {
        match self {
            Here(t) => inject(t),
            There(ts) => ts.embed(),
        }
    }
}

/// Base case: If all constructors are embedded, then the recursion stops.
impl<Us> EmbedIn<Us, Nil> for Void {
    fn embed(self) -> Us {
        match self {}
    }
}

/// Embed the constructors from `Us` into `Ts`.
pub fn embed<Ts, Us: EmbedIn<Ts, impl Nats>>(us: Us) -> Ts {
    Us::embed(us)
}

// Tests ///////////////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_inject() {
        let a: Sum!(u64, f32, bool, String) = inject(32);
        let b: Sum!(u64, f32, bool, String) = inject(32.0);
        let c: Sum!(u64, f32, bool, String) = inject(true);
        let d: Sum!(u64, f32, bool, String) = inject("".to_owned());
        assert_eq!(a, Here(32));
        assert_eq!(b, There(Here(32.0)));
        assert_eq!(c, There(There(Here(true))));
        assert_eq!(d, There(There(There(Here("".to_owned())))));
    }

    #[test]
    fn test_embed() {
        let x: Sum!(u64, f32, bool, String) = Here(32);
        let _: Sum!(u8, u64, u32, f32, u16, bool, String) = x.embed();
    }
}
