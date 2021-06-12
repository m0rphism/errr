#![feature(proc_macro_quote)]

extern crate proc_macro;
extern crate proc_macro2;
extern crate quote;

use std::iter::FromIterator;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;

use syn::{parse_macro_input, ItemFn, ReturnType, Type, TypePath, Path,
          PathArguments, AngleBracketedGenericArguments, Ident, GenericArgument,
          GenericParam, TypeParam, punctuated::Punctuated, token, Token,
          TypeParamBound, parse_quote, Fields, FieldsNamed, FieldsUnnamed, Field,
          Arm, ItemEnum,
};

fn nat(n: usize) -> Type {
    let mut t = parse_quote!(Zero);
    for _ in 0..n {
        t = parse_quote!(Succ<#t>);
    }
    t
}

fn fields_to_type2(fs: &Punctuated<Field, Token!(,)>) -> Type {
    if fs.len() == 1 {
        fs.iter().next().unwrap().ty.clone()
    } else {
        let mut ts = Punctuated::<Type, Token!(,)>::new();
        for f in fs {
            ts.push(f.ty.clone())
        }
        parse_quote!((#ts))
    }
}

fn fields_to_type(fs: &Fields) -> Type {
    match fs {
        Fields::Unnamed(FieldsUnnamed { unnamed, ..}) => fields_to_type2(unnamed),
        Fields::Unit => parse_quote!(()),
        Fields::Named(FieldsNamed { named, ..}) => fields_to_type2(named),
    }
}

/// Derives the [`Has`] trait for each constructor of an `enum`.
///
/// For example for the `enum`
/// ```
/// #[derive(Has)]
/// enum ErrorABC {
///     ErrorA(ErrA),
///     ErrorB(ErrB),
///     ErrorC(ErrC),
/// }
/// ```
/// implementations for
/// `Has<ErrA, Zero>`,
/// `Has<ErrB, Succ<Zero>>`, and
/// `Has<ErrC, Succ<Succ<Zero>>>` are derived.
///
/// The implementation for `Has<ErrA, Zero>` is
/// ```
/// impl Has<ErrA, Zero> for ErrorABC {
///     type WithoutT = Sum!(ErrB, ErrC);
///
///     fn inject(t: ErrA) -> Self {
///         ErrorABC::ErrorA(t)
///     }
///
///     fn pull_out(self) -> Sum<ErrA, Self::WithoutT> {
///         match self {
///             ErrorABC::ErrorA(e) => Here(e),
///             ErrorABC::ErrorB(e) => There(inject(e)),
///             ErrorABC::ErrorC(e) => There(inject(e)),
///         }
///     }
/// }
/// ```
/// The other two implementations are analogous.
#[proc_macro_derive(Has)]
pub fn derive_has(item: TokenStream) -> TokenStream {
    let f = parse_macro_input!(item as ItemEnum);
    let mut out = TokenStream::new();

    let variant_types: Vec<Type> = f.variants.iter().map(|v| fields_to_type(&v.fields)).collect();
    let enum_id = &f.ident;
    for (i, v) in f.variants.iter().enumerate() {
        let v_id = &v.ident;
        let n = nat(i);
        let t = fields_to_type(&v.fields);
        let mut other_variants = variant_types.clone();
        let _ = other_variants.remove(i);
        let other_variants = Punctuated::<Type, Token!(,)>::from_iter(other_variants.clone().into_iter());
        let mut pull_out_arms: Vec<Arm> = vec![];
        // TODO: this doesn't work with variants containing (named) tuples.
        pull_out_arms.push(parse_quote!(#enum_id::#v_id(t) => Here(t),));
        for (j, v) in f.variants.iter().enumerate() {
            if j != i {
                let v_id = &v.ident;
                pull_out_arms.push(parse_quote!(#enum_id::#v_id(t) => There(inject(t)),));
            }
        }
        out.extend::<TokenStream>(quote!(
            impl Has<#t, #n> for #enum_id {
                type WithoutT = Sum!(#other_variants);
                fn inject(t: #t) -> Self {
                    #enum_id::#v_id(t)
                }
                fn pull_out(self) -> Sum<#t, Self::WithoutT> {
                    match self {
                        #(#pull_out_arms)*
                    }
                }
            }
        ).into());
    }

    out
}

fn split_type_app(t: &Type) -> (&Ident, Vec<&Type>) {
    let segments = match t {
        Type::Path(TypePath{ qself: _, path: Path{ leading_colon: _, segments }}) => segments,
        _ => panic!("Invalid return type for errr macro."),
    };
    let segment = segments.first().unwrap();
    let args = match &segment.arguments {
        PathArguments::AngleBracketed(AngleBracketedGenericArguments{args, ..}) => args,
        _ => panic!(),
    };
    let args = args.iter().map(|ga| match ga {
        GenericArgument::Type(t) => t,
        _ => panic!(),
    }).collect();
    (&segment.ident, args)
}

/// Syntax for defining functions which return `Result<T, E>` where `E` is a polymorphic variant.
///
/// For example the function
/// ```
/// #[errr]
/// fn f_ab() -> Result<(), Errors<ErrA, ErrB>> {
///     // ...
/// }
/// ```
/// expands to
/// ```
/// fn f_ab<E: Has<ErrA, impl Nat> + Has<ErrB, impl Nat>>() -> Result<(), E> {
///     // ...
/// }
/// ```
#[proc_macro_attribute]
pub fn errr(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let f = parse_macro_input!(item as ItemFn);

    let mut g = f.clone();

    let ret_type = match f.sig.output {
        ReturnType::Type(_, t) => t,
        ReturnType::Default => panic!("Return type required for errr macro."),
    };

    let (id_result, args) = split_type_app(&ret_type);
    assert_eq!(id_result.to_string().as_str(), "Result");

    let mut iter = args.iter();
    let ok_type = iter.next().unwrap();
    let err_type = iter.next().unwrap();

    let (id_errors, err_types) = split_type_app(err_type);
    assert_eq!(id_errors.to_string().as_str(), "Errors");

    let err_ident = Ident::new("E", Span::call_site());

    let mut bounds: Punctuated<TypeParamBound, token::Add> = Punctuated::new();
    for t in err_types {
        bounds.push_value(parse_quote!(Has<#t, impl Nat>));
        bounds.push_punct(parse_quote!(+));
    }
    let bound = GenericParam::Type(TypeParam {
        attrs: vec![],
        ident: err_ident.clone(),
        colon_token: None,
        bounds,
        eq_token: None,
        default: None,
    });
    if !g.sig.generics.params.is_empty() && !g.sig.generics.params.trailing_punct() {
        g.sig.generics.params.push_punct(parse_quote!(,))
    }
    g.sig.generics.params.push_value(bound);
    g.sig.output = parse_quote!(-> Result<#ok_type, #err_ident>);

    quote!(#g).into()
}
