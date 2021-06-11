use errr_proc::*;

// TODO: give a way to define/derive HSum/Has/Embed instances for real enums. This can be usefull at api boundaries.

/// A Sum-Type like [`Result`], but with different names.
///
/// We can encode an arbitrary enum as a nested [`Sum`], e.g.
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
/// if we nest a [`Sum`] inside a [`Sum`] it always fills the right parameter `B` and never the left parameter `A`.
/// For example, if we construct a value of type `SumError`, then the number of `There` constructors is the index in the type-level list:
/// ```
/// let err_a: SumError = Here(42);                    // Index 0: u64
/// let err_b: SumError = There(Here("".to_owned()));  // Index 1: String
/// let err_c: SumError = There(There(Here(5.2)));     // Index 2: f32
/// ```
///
/// The [`HSum!`] macro gives more readable syntax to write down nested [`Sum`]-Types:
/// ```
/// type SumError = HSum!(u64, String, f32);
/// ```
#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub enum Sum<A, B> {
    Here(A),
    There(B),
}

/// A type with no elements.
///
/// This acts as a neutral element to [`Sum`]:
/// The type [`Sum`]`<A, `[`Void`]`>` is isomorphic to `A`, i.e. while both types have different internal structure,
/// they have the same number of elements with the same meaning and behave the
/// same for all things we're interested in.
#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub enum Void {}

pub use Sum::{Here, There};

pub trait HSum {}
impl HSum for Void {}
impl<T, TS: HSum> HSum for Sum<T, TS> {}

// Macros //////////////////////////////////////////////////////////////////////

pub trait HTrue {}
impl<T> HTrue for T {}

#[macro_export]
macro_rules! HSum {
    ($t:ty , $($ts:tt)*) => {Sum<$t, HSum!($($ts)*)>};
    ($t:ty) => {Sum<$t, Void>};
    () => {Void};
}

#[macro_export]
macro_rules! HFun {
    ($( fn $id:ident ($( $param:ident : $param_t:ty ),*) -> Result<$res_t:ty, Errors<$($err_t:ty),*>> { $($body:tt)* } )*) => {
        $(
            fn $id
                <E: $(Has<$err_t, impl Nat> +)* HTrue>
            ($( $param : $param_t),*) -> Result<$res_t, E> { $($body)* }
        )*
    }
}

#[macro_export]
macro_rules! hmatch_inner {
    ( ($e:expr) { $pat:pat => { $($body:tt)* } $($rest:tt)* } ) => {
        match matches($e) {
            Ok($pat) => { $($body)* }
            Err(rest) => { hmatch_inner!((rest) { $($rest)* }) }
        }
    };
    ( ($e:expr) { } ) => {
        match $e {}
    }
}

#[macro_export]
macro_rules! hmatch {
    ( $e:expr => { $( $err:ident($pat:pat) => { $($body:tt)* } )* } ) => {{
        let e: HSum!($($err),*) = $e;
        hmatch_inner!((e) { $( $err($pat) => { $($body)* } )* })
    }}
}

#[macro_export]
macro_rules! hmatch_res {
    ( $e:expr => { Ok($p:pat) => { $($body:tt)* } $($rest:tt)* } ) => {{
        match $e {
            Ok($p) => { $($body)* }
            Err(e) => hmatch!(e => { $($rest)* })
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

// HHas ////////////////////////////////////////////////////////////////////////

pub trait Has<T, N: Nat>: HSum {
    type WithoutT;
    fn inject(t: T) -> Self;
    fn pull_out(self) -> Sum<T, Self::WithoutT>;
}

pub fn inject<T, TS: Has<T, impl Nat>>(t: T) -> TS {
    TS::inject(t)
}

pub fn matches<T, TS: Has<T, impl Nat>>(ts: TS) -> Result<T, TS::WithoutT> {
    match ts.pull_out() {
        Here(t) => Ok(t),
        There(ts) => Err(ts),
    }
}

impl<T, TS: HSum> Has<T, Zero> for Sum<T, TS> {
    type WithoutT = TS;
    fn inject(t: T) -> Self {
        Here(t)
    }
    fn pull_out(self) -> Sum<T, Self::WithoutT> {
        self
    }
}

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

pub trait EmbedTo<TS, Ns: Nats>: HSum {
    fn embed(self) -> TS;
}

impl<T, N: Nat, NS: Nats, TS: HSum + EmbedTo<US, NS>, US: Has<T, N>> EmbedTo<US, Cons<N, NS>> for Sum<T, TS> {
    fn embed(self) -> US {
        match self {
            Here(t) => inject(t),
            There(ts) => ts.embed(),
        }
    }
}

impl<US> EmbedTo<US, Nil> for Void {
    fn embed(self) -> US {
        match self {}
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_inject() {
        let a: HSum!(u64, f32, bool, String) = inject(32);
        let b: HSum!(u64, f32, bool, String) = inject(32.0);
        let c: HSum!(u64, f32, bool, String) = inject(true);
        let d: HSum!(u64, f32, bool, String) = inject("".to_owned());
        assert_eq!(a, Here(32));
        assert_eq!(b, There(Here(32.0)));
        assert_eq!(c, There(There(Here(true))));
        assert_eq!(d, There(There(There(Here("".to_owned())))));
    }

    #[test]
    fn test_embed() {
        let x: HSum!(u64, f32, bool, String) = Here(32);
        let _: HSum!(u8, u64, u32, f32, u16, bool, String) = x.embed();
    }
}
