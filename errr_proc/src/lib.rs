#![feature(proc_macro_quote)]

extern crate proc_macro;
extern crate proc_macro2;
extern crate quote;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;

use syn::{parse_macro_input, ItemFn, ReturnType, Type, TypePath, Path,
          PathArguments, AngleBracketedGenericArguments, Ident, GenericArgument,
          GenericParam, TypeParam, punctuated::Punctuated, token,
          TypeParamBound, parse_quote
};

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
