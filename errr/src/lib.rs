pub use errr_proc::*;

// TODO: give a way to define/derive Sum/Has/Embed instances for real enums. This can be usefull at api boundaries.

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
/// The constructor names reflect that we encode enums always as a list and never as a real tree, i.e.
/// if we nest a [`enum@Sum`] inside a [`enum@Sum`] it always fills the right parameter `B` and never the left parameter `A`.
/// For example, if we construct a value of type `SumError`, then the number of `There` constructors is the index in the type-level list:
/// ```
/// let err_a: SumError = Here(42);                    // Index 0: u64
/// let err_b: SumError = There(Here("".to_owned()));  // Index 1: String
/// let err_c: SumError = There(There(Here(5.2)));     // Index 2: f32
/// ```
///
/// The [`Sum!`] macro gives more readable syntax to write down nested [`enum@Sum`]-Types:
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
/// The type [`enum@Sum`]`<A, `[`Void`]`>` is isomorphic to `A`, i.e. while both types have different internal structure,
/// they have the same number of elements with the same meaning and behave the
/// same for all things we're interested in.
#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub enum Void {}

pub use Sum::{Here, There};

// Macros //////////////////////////////////////////////////////////////////////

#[macro_export]
macro_rules! Sum {
    ($t:ty , $($ts:tt)*) => {Sum<$t, Sum!($($ts)*)>};
    ($t:ty) => {Sum<$t, Void>};
    () => {Void};
}

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

#[macro_export]
macro_rules! match_sum {
    ( $e:expr => { $( $err:ident($pat:pat) => { $($body:tt)* } )* } ) => {{
        let e: Sum!($($err),*) = $e;
        match_sum_inner!((e) { $( $err($pat) => { $($body)* } )* })
    }}
}

#[macro_export]
macro_rules! match_sum_res {
    ( $e:expr => { Ok($p:pat) => { $($body:tt)* } $($rest:tt)* } ) => {{
        match $e {
            Ok($p) => { $($body)* }
            Err(e) => match_sum!(e => { $($rest)* })
        }
    }}
}

// Nats ////////////////////////////////////////////////////////////////////////

pub trait Nat {}

pub struct Zero {}
pub struct Succ<N: Nat> {
    phantom: std::marker::PhantomData<N>,
}

impl Nat for Zero {}
impl<N: Nat> Nat for Succ<N> {}

pub trait Nats {}

pub struct Nil {}
pub struct Cons<N: Nat, Ns: Nats> {
    phantom_n: std::marker::PhantomData<N>,
    phantom_ns: std::marker::PhantomData<Ns>,
}

impl Nats for Nil {}
impl<Ns: Nats, N: Nat> Nats for Cons<N, Ns> {}

// Has /////////////////////////////////////////////////////////////////////////

/// Types which behave like an enum whose `N`-th constructor contains type `T`.
/// If there is only one constructor of type `T`, then the compiler can infer
/// the `N`.
pub trait Has<T, N: Nat> {
    /// A type which has all the constructors of `Self` except the one for `T`.
    type WithoutT;
    /// Inject a `T` into `Self` by using its constructor.
    fn inject(t: T) -> Self;
    /// Match on `Self` and return either a `T` or a value of an enum which
    /// could be any other constructor of `Self` except for `T`.
    fn pull_out(self) -> Sum<T, Self::WithoutT>;
}

/// Inject a `T` into `TS` by using its constructor.
pub fn inject<T, TS: Has<T, impl Nat>>(t: T) -> TS {
    TS::inject(t)
}

/// Match on `Self` and return either a `T` or a value of an enum which
/// could be any other constructor of `Self` except for `T`.
pub fn matches<T, TS: Has<T, impl Nat>>(ts: TS) -> Result<T, TS::WithoutT> {
    match ts.pull_out() {
        Here(t) => Ok(t),
        There(ts) => Err(ts),
    }
}

/// If `T` is the first constructor of `Self`.
impl<T, TS> Has<T, Zero> for Sum<T, TS> {
    type WithoutT = TS;
    fn inject(t: T) -> Self {
        Here(t)
    }
    fn pull_out(self) -> Sum<T, Self::WithoutT> {
        self
    }
}

/// If `T` is not the first constructor of `Self`.
impl<T, U, TS: Has<T, N>, N: Nat> Has<T, Succ<N>> for Sum<U, TS> {
    type WithoutT = Sum<U, TS::WithoutT>;
    fn inject(t: T) -> Self {
        There(TS::inject(t))
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

// EmbedTo /////////////////////////////////////////////////////////////////////

/// If we have some enum `Self`, which has a subset of the constructors of some
/// enum `TS`, then we can convert `Self` to `TS`.
///
/// `Ns` is a list of type level natural numbers, which describes for each
/// constructor from `Self` at which index the corresponding constructor in `TS` is.
///
/// `Ns` is inferred by the compiler, if the constructor types are unique.
pub trait EmbedTo<TS, Ns: Nats> {
    fn embed(self) -> TS;
}

/// Recursive case: Try to embed the first constructor from `Self` otherwise
/// recursively embed the rest.
impl<T, N: Nat, NS: Nats, TS: EmbedTo<US, NS>, US: Has<T, N>> EmbedTo<US, Cons<N, NS>> for Sum<T, TS> {
    fn embed(self) -> US {
        match self {
            Here(t) => inject(t),
            There(ts) => ts.embed(),
        }
    }
}

/// Base case: If all constructors are embedded, then the recursion stops.
impl<US> EmbedTo<US, Nil> for Void {
    fn embed(self) -> US {
        match self {}
    }
}

pub fn embed<TS, US: EmbedTo<TS, impl Nats>>(us: US) -> TS {
    US::embed(us)
}

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
