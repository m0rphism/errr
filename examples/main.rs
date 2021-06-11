#![allow(dead_code)]

use errr::*;

struct ErrA(String);
struct ErrB(String);
struct ErrC(String);
struct ErrD(String);

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

    fn f_ab_0() -> Result<(), HSum!(ErrA, ErrB)> {
        f_a().map_err(inject)?;
        f_b().map_err(inject)?;
        Ok(())
    }

    fn f_ab_1() -> Result<(), HSum!(ErrA, ErrB)> {
        f_a().map_err(inject)?;
        f_b().map_err(inject)?;
        Ok(())
    }

    fn f_ab_2() -> Result<(), HSum!(ErrD, ErrB, ErrA, ErrC)> {
        f_a().map_err(inject)?;
        f_b().map_err(inject)?;
        Ok(())
    }

    fn f_ab_3<M: Nat, N: Nat, E: Has<ErrA, M> + Has<ErrB, N>>() -> Result<(), E> {
        f_a().map_err(inject)?;
        f_b().map_err(inject)?;
        Ok(())
    }
}

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

    fn f_ab_1() -> Result<(), HSum!(ErrA, ErrB)> {
        f_a()?;
        f_b()?;
        Ok(())
    }

    fn f_ab_2() -> Result<(), HSum!(ErrD, ErrB, ErrA, ErrC)> {
        f_a()?;
        f_b()?;
        Ok(())
    }

    fn f_ab_3<E: Has<ErrA, impl Nat> + Has<ErrB, impl Nat>>() -> Result<(), E> {
        f_a()?;
        f_b()?;
        Ok(())
    }

    fn f_abc_1() -> Result<(), HSum!(ErrA, ErrB, ErrC)> {
        f_ab_3()?;
        f_c()?;
        Ok(())
    }

    fn f_abc_3<E: Has<ErrA, impl Nat> + Has<ErrB, impl Nat> + Has<ErrC, impl Nat>>() -> Result<(), E> {
        f_ab_3()?;
        f_c()?;
        Ok(())
    }

    fn handle_f_abc_3() {
        let res: Result<(), HSum!(ErrA, ErrB, ErrC)> = f_abc_3();
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

    fn handle_f_abc_4() {
        let res: Result<(), HSum!(ErrA, ErrB, ErrC)> = f_abc_3();
        match res {
            Ok(()) => (),
            Err(e) => match matches(e) {
                Ok(ErrA(_s)) => todo!(),
                Err(_e) => todo!(),
            },
        }
    }
}

mod test3 {
    use super::*;

    HFun! {
        fn f_a() -> Result<(), Errors<ErrA>> {
            Ok(())
        }
        fn f_b() -> Result<(), Errors<ErrB>> {
            Ok(())
        }
        fn f_c() -> Result<(), Errors<ErrC>> {
            Ok(())
        }
        fn f_d() -> Result<(), Errors<ErrD>> {
            Ok(())
        }

        fn f_ab_1() -> Result<(), Errors<ErrA, ErrB>> {
            f_a()?;
            f_b()?;
            Ok(())
        }

        fn f_ab_2() -> Result<(), Errors<ErrD, ErrB, ErrA, ErrC>> {
            f_a()?;
            f_b()?;
            Ok(())
        }

        fn f_abc_1() -> Result<(), Errors<ErrA, ErrB, ErrC>> {
            f_ab_1()?;
            f_c()?;
            Ok(())
        }
    }

    fn handle_f_abc() {
        match f_abc_1() {
            Ok(()) => (),
            Err(e) => hmatch!(e => {
                ErrA(e) => { println!("ErrA: {}", e) }
                ErrB(e) => { println!("ErrB: {}", e) }
                ErrC(e) => { println!("ErrC: {}", e) }
            })
        }
    }

    fn handle_f_abc_2() {
        hmatch_res!(f_abc_1() => {
            Ok(()) => {}
            ErrA(e) => { println!("ErrA: {}", e) }
            ErrB(e) => { println!("ErrB: {}", e) }
            ErrC(e) => { println!("ErrC: {}", e) }
        })
    }

}

fn main() {
    println!("Hello, world!");
}

