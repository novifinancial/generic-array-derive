// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

//! # Derive macros for opaque-ke

use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned};
use syn::{
    parse_quote, spanned::Spanned, Data, DeriveInput, Fields, GenericParam, Generics, Index,
};

//////////////////////////
// TryFromForSizedBytes //
//////////////////////////

/// Derive TryFrom<&[u8], Error = ErrorType> for any T: SizedBytes, assuming
/// ErrorType: Default. This proc-macro is here to work around the lack of
/// specialization, but there's nothing otherwise clever about it.
#[proc_macro_derive(TryFromForSizedBytes, attributes(ErrorType))]
pub fn try_from_for_sized_bytes(source: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast: DeriveInput = syn::parse(source).expect("Incorrect macro input");
    let name = &ast.ident;

    let error_type = get_type_from_attrs(&ast.attrs, "ErrorType").unwrap();

    let generics = add_basic_bound(ast.generics);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let gen = quote! {
        impl #impl_generics ::std::convert::TryFrom<&[u8]> for #name #ty_generics #where_clause {
            type Error = #error_type;

            fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
                let expected_len = <<Self as ::generic_bytes::SizedBytes>::Len as generic_array::typenum::Unsigned>::to_usize();
                if bytes.len() != expected_len {
                    return Err(#error_type::default());
                }
                let arr = GenericArray::from_slice(bytes);
                <Self as ::generic_bytes::SizedBytes>::from_arr(arr).map_err(|_| #error_type::default())
            }
        }
    };
    gen.into()
}

fn get_type_from_attrs(attrs: &[syn::Attribute], attr_name: &str) -> syn::Result<syn::Type> {
    attrs
        .iter()
        .find(|attr| attr.path.is_ident(attr_name))
        .map_or_else(
            || {
                Err(syn::Error::new(
                    proc_macro2::Span::call_site(),
                    format!("Could not find attribute {}", attr_name),
                ))
            },
            |attr| match attr.parse_meta()? {
                syn::Meta::NameValue(meta) => {
                    if let syn::Lit::Str(lit) = &meta.lit {
                        Ok(lit.clone())
                    } else {
                        Err(syn::Error::new_spanned(
                            meta,
                            &format!("Could not parse {} attribute", attr_name)[..],
                        ))
                    }
                }
                bad => Err(syn::Error::new_spanned(
                    bad,
                    &format!("Could not parse {} attribute", attr_name)[..],
                )),
            },
        )
        .and_then(|str| str.parse())
}

// add `T: SizedBytes` to each generic parameter
fn add_basic_bound(mut generics: Generics) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            type_param
                .bounds
                .push(parse_quote!(::generic_bytes::SizedBytes));
        }
    }
    generics
}

////////////////
// SizedBytes //
////////////////

// add where cause which reflects the bound propagation for generic SizedBytes clauses
fn add_trait_bounds(
    generics: &mut Generics,
    data: &syn::Data,
    bound: syn::Path,
) -> Result<(), syn::Error> {
    if generics.params.is_empty() {
        return Ok(());
    }

    let types = collect_types(&data)?;
    if !types.is_empty() {
        let where_clause = generics.make_where_clause();

        types
            .into_iter()
            .for_each(|ty| where_clause.predicates.push(parse_quote!(#ty : #bound)));
        bounds_sum(data, where_clause)?;
    }

    Ok(())
}

fn collect_types(data: &syn::Data) -> Result<Vec<syn::Type>, syn::Error> {
    use syn::*;

    let types = match *data {
        Data::Struct(ref data) => match &data.fields {
            Fields::Named(FieldsNamed { named: fields, .. })
            | Fields::Unnamed(FieldsUnnamed {
                unnamed: fields, ..
            }) => fields.iter().map(|f| f.ty.clone()).collect(),

            Fields::Unit => Vec::new(),
        },

        Data::Enum(ref data) => data
            .variants
            .iter()
            .flat_map(|variant| match &variant.fields {
                Fields::Named(FieldsNamed { named: fields, .. })
                | Fields::Unnamed(FieldsUnnamed {
                    unnamed: fields, ..
                }) => fields.iter().map(|f| f.ty.clone()).collect(),

                Fields::Unit => Vec::new(),
            })
            .collect(),

        Data::Union(_) => {
            return Err(Error::new(
                Span::call_site(),
                "Union types are not supported.",
            ))
        }
    };

    Ok(types)
}

fn extract_size_type_from_generic_array(ty: &syn::Type) -> Option<&syn::Type> {
    fn path_is_generic_array(path: &syn::Path) -> Option<&syn::GenericArgument> {
        path.segments.iter().find_map(|pt| {
            if pt.ident == "GenericArray" {
                // It should have only on angle-bracketed param ("<Foo, Bar>"):
                match &pt.arguments {
                    syn::PathArguments::AngleBracketed(params) if params.args.len() == 2 => {
                        params.args.last()
                    }
                    _ => None,
                }
            } else {
                None
            }
        })
    }

    match ty {
        syn::Type::Path(typepath)
            if typepath.qself.is_none()
                && typepath
                    .path
                    .segments
                    .iter()
                    .any(|pt| pt.ident == "GenericArray") =>
        {
            // Get the second parameter of the GenericArray
            let type_param = path_is_generic_array(&typepath.path);
            // This argument must be a type:
            if let Some(syn::GenericArgument::Type(ty)) = type_param {
                Some(ty)
            } else {
                None
            }
        }
        _ => None,
    }
}

fn bounds_sum(data: &Data, where_clause: &mut syn::WhereClause) -> Result<(), syn::Error> {
    match *data {
        Data::Struct(ref data) => {
            match data.fields {
                Fields::Named(ref fields) => {
                    let mut quote = None;
                    for f in fields.named.iter() {
                        let ty = &f.ty;
                        let res =
                            if let Some(unsigned_ty) = extract_size_type_from_generic_array(ty) {
                                quote_spanned! {f.span()=>
                                                #unsigned_ty
                                }
                            } else {
                                quote_spanned! {f.span()=>
                                                <#ty as ::generic_bytes::SizedBytes>::Len
                                }
                            };
                        if let Some(ih) = quote {
                            quote = Some(quote! {
                                ::generic_array::typenum::Sum<#ih, #res>
                            });
                            where_clause
                                .predicates
                                .push(parse_quote!(#ih: ::core::ops::Add<#res>));
                            where_clause
                                .predicates
                                .push(parse_quote!(::generic_array::typenum::Sum<#ih, #res> : ::generic_array::ArrayLength<u8> + ::core::ops::Sub<#ih, Output = #res>));
                            where_clause
                                .predicates
                                .push(parse_quote!(::generic_array::typenum::Diff<::generic_array::typenum::Sum<#ih, #res>, #ih> : ::generic_array::ArrayLength<u8>));
                        } else {
                            quote = Some(res);
                        }
                    }
                    Ok(())
                }
                Fields::Unnamed(ref fields) => {
                    let mut quote = None;
                    for f in fields.unnamed.iter() {
                        let ty = &f.ty;
                        let res =
                            if let Some(unsigned_ty) = extract_size_type_from_generic_array(ty) {
                                quote_spanned! {f.span()=>
                                                #unsigned_ty
                                }
                            } else {
                                quote_spanned! {f.span()=>
                                                <#ty as ::generic_bytes::SizedBytes>::Len
                                }
                            };
                        if let Some(ih) = quote {
                            quote = Some(quote! {
                                ::generic_array::typenum::Sum<#ih, #res>
                            });
                            where_clause
                                .predicates
                                .push(parse_quote!(#ih : ::core::ops::Add<#res>));
                            where_clause
                                .predicates
                                .push(parse_quote!(::generic_array::typenum::Sum<#ih, #res> : ::generic_array::ArrayLength<u8> + ::core::ops::Sub<#ih, Output = #res>));
                            where_clause
                                .predicates
                                .push(parse_quote!(::generic_array::typenum::Diff<::generic_array::typenum::Sum<#ih, #res>, #ih> : ::generic_array::ArrayLength<u8>));
                        } else {
                            quote = Some(res);
                        }
                    }
                    Ok(())
                }
                Fields::Unit => {
                    // Unit structs cannot own more than 0 bytes of heap memory.
                    unimplemented!()
                }
            }
        }
        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    }
}

// create a type expression summing up the ::Len of each field
fn sum(data: &Data) -> TokenStream {
    match *data {
        Data::Struct(ref data) => {
            match data.fields {
                Fields::Named(ref fields) => {
                    let mut quote = None;
                    for f in fields.named.iter() {
                        let ty = &f.ty;
                        let res = quote_spanned! {f.span()=>
                            <#ty as ::generic_bytes::SizedBytes>::Len
                        };
                        if let Some(ih) = quote {
                            quote = Some(quote! {
                                ::generic_array::typenum::Sum<#ih, #res>
                            });
                        } else {
                            quote = Some(res);
                        }
                    }
                    quote! {
                        #quote
                    }
                }
                Fields::Unnamed(ref fields) => {
                    let mut quote = None;
                    for f in fields.unnamed.iter() {
                        let ty = &f.ty;
                        let res = quote_spanned! {f.span()=>
                            <#ty as ::generic_bytes::SizedBytes>::Len
                        };
                        if let Some(ih) = quote {
                            quote = Some(quote! {
                                ::generic_array::typenum::Sum<#ih, #res>
                            });
                        } else {
                            quote = Some(res);
                        }
                    }
                    quote! {
                        #quote
                    }
                }
                Fields::Unit => {
                    // Unit structs cannot own more than 0 bytes of heap memory.
                    unimplemented!()
                }
            }
        }
        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    }
}

// Generate an expression to concatenate the to_arr of each field
fn byte_concatenation(data: &Data) -> TokenStream {
    match *data {
        Data::Struct(ref data) => {
            match data.fields {
                Fields::Named(ref fields) => {
                    let mut quote = None;
                    for f in fields.named.iter() {
                        let name = &f.ident;
                        let res = quote_spanned! {f.span()=>
                            ::generic_bytes::SizedBytes::to_arr(&self.#name)
                        };
                        if let Some(ih) = quote {
                            quote = Some(quote! {
                                ::generic_array::sequence::Concat::concat(#ih, #res)
                            });
                        } else {
                            quote = Some(res);
                        }
                    }
                    quote! {
                        #quote
                    }
                }
                Fields::Unnamed(ref fields) => {
                    let mut quote = None;
                    for (i, f) in fields.unnamed.iter().enumerate() {
                        let index = Index::from(i);
                        let res = quote_spanned! {f.span()=>
                            ::generic_bytes::SizedBytes::to_arr(&self.#index)
                        };
                        if let Some(ih) = quote {
                            quote = Some(quote! {
                                ::generic_array::sequence::Concat::concat(#ih, #res)
                            });
                        } else {
                            quote = Some(res);
                        }
                    }
                    quote! {
                        #quote
                    }
                }
                Fields::Unit => {
                    // Unit structs cannot own more than 0 bytes of heap memory.
                    quote!(0)
                }
            }
        }
        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    }
}

// Generate an expression to concatenate the to_arr of each field
fn byte_splitting(constr: &proc_macro2::Ident, data: &Data) -> TokenStream {
    match *data {
        Data::Struct(ref data) => {
            match data.fields {
                Fields::Named(ref fields) => {
                    let l = fields.named.len();
                    let setup: TokenStream = fields
                        .named
                        .iter().enumerate()
                        .map(|(i, f)| {
                            let name = &f.ident;
                            let ty = &f.ty;

                            if i < (l-1) {
                                quote_spanned! {f.span()=>
                                    let (head, _tail): (&GenericArray<u8, <#ty as ::generic_bytes::SizedBytes>::Len>, &GenericArray<u8, _>) =
                                                generic_array::sequence::Split::split(_tail);
                                    let #name: #ty = ::generic_bytes::SizedBytes::from_arr(head)?;
                                }
                            } else {
                                quote_spanned!{f.span() =>
                                    let #name: #ty = ::generic_bytes::SizedBytes::from_arr(_tail)?;
                                }
                            }
                        })
                        .collect();

                    let conclude: TokenStream = fields
                        .named
                        .iter()
                        .map(|f| {
                            let name = &f.ident;
                            quote_spanned! {f.span()=>
                                #name,
                            }
                        })
                        .collect();
                    quote! {
                        let _tail = arr;
                        #setup
                        Ok(#constr {
                            #conclude
                        })
                    }
                }
                Fields::Unnamed(ref fields) => {
                    let l = fields.unnamed.len();
                    let setup: TokenStream = fields
                        .unnamed
                        .iter()
                        .enumerate()
                        .map(|(i, f)| {
                            let ty = &f.ty;
                            if i < (l-1) {
                                let field_name = format!("f_{}", i);
                                let fname = syn::Ident::new(&field_name, f.span());
                                quote_spanned! {f.span()=>
                                                let (head, _tail) = generic_array::sequence::Split::split(_tail);
                                                let #fname: #ty = ::generic_bytes::SizedBytes::from_arr(head)?;
                                }
                            } else {
                                let field_name = format!("f_{}", i);
                                let fname = syn::Ident::new(&field_name, f.span());
                                quote_spanned! {f.span()=>
                                                let #fname: #ty = ::generic_bytes::SizedBytes::from_arr(_tail)?;
                                }
                            }
                        })
                        .collect();

                    let conclude: TokenStream = fields
                        .unnamed
                        .iter()
                        .enumerate()
                        .map(|(i, f)| {
                            let field_name = format!("f_{}", i);
                            let fname = syn::Ident::new(&field_name, f.span());
                            quote_spanned! {f.span()=>
                                #fname,
                            }
                        })
                        .collect();
                    quote! (
                        let _tail = arr;
                        #setup
                        Ok(#constr (
                            #conclude
                        ))
                    )
                }
                Fields::Unit => {
                    // Unit structs cannot own more than 0 bytes of heap memory.
                    quote!(0)
                }
            }
        }
        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    }
}

#[proc_macro_derive(SizedBytes)]
pub fn derive_sized_bytes(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut input: DeriveInput = match syn::parse(input) {
        Ok(input) => input,
        Err(e) => return e.to_compile_error().into(),
    };
    let name = &input.ident;

    // Add a bound `T::From : SizedBytes` to every type parameter occurrence `T::From`.
    if let Err(e) = add_trait_bounds(
        &mut input.generics,
        &input.data,
        parse_quote!(::generic_bytes::SizedBytes),
    ) {
        return e.to_compile_error().into();
    };

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    // Generate an expression to sum the type lengths of each field.
    let types_sum = sum(&input.data);

    // Generate an expression to concatenate each field.
    let to_arr_impl = byte_concatenation(&input.data);

    // Generate an expression to ingest each field.
    let from_arr_impl = byte_splitting(name, &input.data);

    let res = quote! (
        // The generated impl.
        impl #impl_generics ::generic_bytes::SizedBytes for #name #ty_generics #where_clause {

            type Len = #types_sum;

            fn to_arr(&self) -> GenericArray<u8, Self::Len> {
                #to_arr_impl
            }

            fn from_arr(arr: &GenericArray<u8, Self::Len>) -> Result<Self, ::generic_bytes::TryFromSizedBytesError> {
                #from_arr_impl
            }
        }
    );
    res.into()
}
