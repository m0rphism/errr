#![allow(dead_code)]

use errr::*;

struct ErrA(String);
struct ErrB(String);
struct ErrC(String);
struct ErrD(String);

/// Explicit
mod test1 {
    use super::*;

    fn f_a() -> Result<(), ErrA> {
        Ok(())
    }

    fn f_b() -> Result<(), ErrB> {
        Ok(())
    }

    fn f_c() -> Result<(), ErrC> {
        Ok(())
    }

    fn f_d() -> Result<(), ErrD> {
        Ok(())
    }

    fn f_ab_1() -> Result<(), Sum!(ErrA, ErrB)> {
        f_a().map_err(inject)?;
        f_b().map_err(inject)?;
        Ok(())
    }

    // Stuff still works if we use an error type with more possibilities.
    fn f_ab_2() -> Result<(), Sum!(ErrD, ErrB, ErrA, ErrC)> {
        f_a().map_err(inject)?;
        f_b().map_err(inject)?;
        Ok(())
    }

    // Deeper nesting requires embedding, because we always chose a concrete
    // Error type via `Sum!`.
    fn f_abc() -> Result<(), Sum!(ErrA, ErrB, ErrC)> {
        f_ab_1().map_err(embed)?;
        f_c().map_err(inject)?;
        Ok(())
    }
}

/// Automatic composition via polymorphism over error type E.
/// No embedding required.
mod test2 {
    use super::*;

    fn f_a_explicit<N: Nat, E: Has<ErrA, N>>() -> Result<(), E> {
        Ok(())
    }

    fn f_a<E: Has<ErrA, impl Nat>>() -> Result<(), E> {
        Ok(())
    }

    fn f_b<E: Has<ErrB, impl Nat>>() -> Result<(), E> {
        Ok(())
    }

    fn f_c<E: Has<ErrC, impl Nat>>() -> Result<(), E> {
        Ok(())
    }

    fn f_d<E: Has<ErrD, impl Nat>>() -> Result<(), E> {
        Ok(())
    }

    fn f_ab<E: Has<ErrA, impl Nat> + Has<ErrB, impl Nat>>() -> Result<(), E> {
        f_a()?;
        f_b()?;
        Ok(())
    }

    fn f_abc<E: Has<ErrA, impl Nat> + Has<ErrB, impl Nat> + Has<ErrC, impl Nat>>() -> Result<(), E> {
        f_ab()?;
        f_c()?;
        Ok(())
    }

    // Matching complains if we miss a case!
    fn handle_f_abc_3() {
        let res: Result<(), Sum!(ErrA, ErrB, ErrC)> = f_abc();
        match res {
            Ok(()) => (),
            Err(e) => match e {
                Sum::Here(_err_a) => todo!(),
                Sum::There(Sum::Here(_err_b)) => todo!(),
                Sum::There(Sum::There(Sum::Here(_err_c))) => todo!(),
                Sum::There(Sum::There(Sum::There(_void))) => unreachable!(),
            },
        }
    }

    // We can also match on a single error and if the match fails,
    // we get a smaller error type to rethrow.
    fn handle_f_abc_4() {
        let res: Result<(), Sum!(ErrA, ErrB, ErrC)> = f_abc();
        match res {
            Ok(()) => (),
            Err(e) => match matches(e) {
                Ok(ErrA(_err_a)) => todo!(),
                Err(_err_bc) => todo!(),
            },
        }
    }
}

/// Hiding Details behind macros.
mod test3 {
    use super::*;

    #[errr]
    fn f_a() -> Result<(), Errors<ErrA>> {
        Ok(())
    }

    #[errr]
    fn f_b() -> Result<(), Errors<ErrB>> {
        Ok(())
    }

    #[errr]
    fn f_c() -> Result<(), Errors<ErrC>> {
        Ok(())
    }

    #[errr]
    fn f_d() -> Result<(), Errors<ErrD>> {
        Ok(())
    }

    #[errr]
    fn f_ab() -> Result<(), Errors<ErrA, ErrB>> {
        f_a()?;
        f_b()?;
        Ok(())
    }

    #[errr]
    fn f_bc() -> Result<(), Errors<ErrB, ErrC>> {
        f_b()?;
        f_c()?;
        Ok(())
    }

    #[errr]
    fn f_abc() -> Result<(), Errors<ErrA, ErrB, ErrC>> {
        f_ab()?;
        f_bc()?;
        Ok(())
    }

    // Macro still works if we add our own generics.
    #[errr]
    fn f_abC<C>(c: C) -> Result<(), Errors<ErrA, ErrB, C>> {
        f_ab()?;
        Err(inject(c))?;
        Ok(())
    }

    fn handle_f_abc() {
        match f_abc() {
            Ok(()) => (),
            Err(e) => match_sum!(e => {
                ErrA(e) => { println!("ErrA: {}", e) }
                ErrB(e) => { println!("ErrB: {}", e) }
                ErrC(e) => { println!("ErrC: {}", e) }
            })
        }
    }

    fn handle_f_abc_2() {
        match_sum_res!(f_abc() => {
            Ok(()) => {}
            ErrA(e) => { println!("ErrA: {}", e) }
            ErrB(e) => { println!("ErrB: {}", e) }
            ErrC(e) => { println!("ErrC: {}", e) }
        })
    }

    // Use own enum to instantiate the polymorphic variant. (WITHOUT MACROS)
    mod custom_variants_1 {
        use super::*;

        enum ErrorABC {
            ErrorA(ErrA),
            ErrorB(ErrB),
            ErrorC(ErrC),
        }

        impl Has<ErrA, Zero> for ErrorABC {
            type WithoutT = Sum!(ErrB, ErrC);

            fn inject(t: ErrA) -> Self {
                ErrorABC::ErrorA(t)
            }

            fn pull_out(self) -> Sum<ErrA, Self::WithoutT> {
                match self {
                    ErrorABC::ErrorA(e) => Here(e),
                    ErrorABC::ErrorB(e) => There(inject(e)),
                    ErrorABC::ErrorC(e) => There(inject(e)),
                }
            }
        }
        impl Has<ErrB, Succ<Zero>> for ErrorABC {
            type WithoutT = Sum!(ErrA, ErrC);

            fn inject(t: ErrB) -> Self {
                ErrorABC::ErrorB(t)
            }

            fn pull_out(self) -> Sum<ErrB, Self::WithoutT> {
                match self {
                    ErrorABC::ErrorA(e) => There(inject(e)),
                    ErrorABC::ErrorB(e) => Here(e),
                    ErrorABC::ErrorC(e) => There(inject(e)),
                }
            }
        }
        impl Has<ErrC, Succ<Succ<Zero>>> for ErrorABC {
            type WithoutT = Sum!(ErrA, ErrB);

            fn inject(t: ErrC) -> Self {
                ErrorABC::ErrorC(t)
            }

            fn pull_out(self) -> Sum<ErrC, Self::WithoutT> {
                match self {
                    ErrorABC::ErrorA(e) => There(inject(e)),
                    ErrorABC::ErrorB(e) => There(inject(e)),
                    ErrorABC::ErrorC(e) => Here(e),
                }
            }
        }
        fn handle_f_abc() {
            match f_abc() {
                Ok(()) => todo!(),
                Err(ErrorABC::ErrorA(_e)) => todo!(),
                Err(ErrorABC::ErrorB(_e)) => todo!(),
                Err(ErrorABC::ErrorC(_e)) => todo!(),
            }
        }
    }

    // Use own enum to instantiate the polymorphic variant. (WITH MACROS)
    mod custom_variants_2 {
        use super::*;

        #[derive(Has)]
        enum ErrorABC {
            ErrorA(ErrA),
            ErrorB(ErrB),
            ErrorC(ErrC),
        }

        fn handle_f_abc() {
            match f_abc() {
                Ok(()) => todo!(),
                Err(ErrorABC::ErrorA(_e)) => todo!(),
                Err(ErrorABC::ErrorB(_e)) => todo!(),
                Err(ErrorABC::ErrorC(_e)) => todo!(),
            }
        }
    }
}

fn main() {
    println!("Hello, world!");
}
