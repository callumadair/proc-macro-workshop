use quote::{
    ToTokens,
    quote,
};
use syn::{
    Attribute,
    Data,
    DeriveInput,
    Field,
    GenericArgument,
    Ident,
    Path,
    PathArguments,
    Type,
    TypePath,
    parse_macro_input,
};

use crate::error::BuilderMacroError;

mod error;

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream
{
    let DeriveInput {
        attrs: _,
        vis: _,
        ident,
        generics: _,
        data,
    } = parse_macro_input!(input as DeriveInput);

    let builder_name = quote::format_ident!("{}Builder", ident);
    let builder_error = quote::format_ident!("{}BuildError", ident);
    let BuilderFieldsAndMethods {
        field_names,
        field_types,
        builder_methods,
    } = create_builder_fields_and_methods(data);

    let out = quote! {
        impl #ident {
            pub fn builder() -> #builder_name {
                #builder_name::default()
            }
        }

        struct #builder_name {
            #( #field_names: core::option::Option<#field_types>, )*
        }

        impl std::default::Default for #builder_name {
            fn default() -> Self {
                Self {
                    ..Default::default()
                }
            }
        }

        #[derive(Debug)]
        struct #builder_error;

        impl #builder_name {
            fn build(&mut self) -> core::result::Result<#ident, #builder_error> {
                #(
                    let #field_names = self.#field_names
                        .as_ref()
                        .ok_or_else(|| #builder_error)?
                        .clone();
                )*
                Ok(#ident {
                    #(#field_names,)*
                })
            }

            #(#builder_methods)*
        }
    };
    out.into_token_stream().into()
}

fn _parse_attrs(_attrs: Vec<Attribute>) -> () { todo!() }

struct BuilderFieldsAndMethods
{
    field_names:     Vec<proc_macro2::TokenStream>,
    field_types:     Vec<proc_macro2::TokenStream>,
    builder_methods: Vec<proc_macro2::TokenStream>,
}

fn create_builder_fields_and_methods(data: Data) -> BuilderFieldsAndMethods
{
    match data
    {
        Data::Struct(data_struct) =>
        {
            match data_struct.fields
            {
                syn::Fields::Named(fields_named) =>
                {
                    let (field_names, field_types, builder_methods) = fields_named
                        .named
                        .iter()
                        .map(|field| {
                            let field_ident = field.ident.clone().unwrap();
                            let field_ty = field.ty.clone();
                            (
                                quote! {
                                    #field_ident
                                },
                                quote! {
                                  #field_ty
                                },
                                // We need to do error handling here, but for now just unwrap.
                                match construct_builder_method(&field)
                                {
                                    Ok(tokens) => tokens,
                                    Err(BuilderMacroError::SynError(syn_error)) =>
                                    {
                                        syn_error.to_compile_error()
                                    }
                                    Err(inner_error) =>
                                    {
                                        let error_string = inner_error.to_string();
                                        quote! {
                                            ::core::compile_error!(#error_string);
                                        }
                                    }
                                },
                            )
                        })
                        .collect();
                    BuilderFieldsAndMethods {
                        field_names,
                        field_types,
                        builder_methods,
                    }
                }
                syn::Fields::Unnamed(_) | syn::Fields::Unit =>
                {
                    unimplemented!("We only support structs with named fields.")
                }
            }
        }
        Data::Enum(_) | Data::Union(_) =>
        {
            unimplemented!("We do not support a builder macro for any type other than a struct.")
        }
    }
}

fn construct_builder_method(field: &Field) -> error::Result<proc_macro2::TokenStream>
{
    let field_name = field.ident.as_ref().ok_or(BuilderMacroError::MissingIdent(
        "No ident found for field type",
    ))?;
    let field_type = &field.ty;
    let out = match extract_field_type_kind(field)?
    {
        FieldKind::Field =>
        {
            // We have to be careful here, as if we used
            // [`_ident`] here, we would get something like
            // `Vec` instead of `Vec<T>`.
            quote! {
                fn #field_name(&mut self, #field_name: #field_type) -> &mut Self {
                    self.#field_name = Some(#field_name);
                    self
                }
            }
        }
        FieldKind::OptionalField { arg_ty_ident } =>
        {
            quote! {
                fn #field_name(&mut self, #field_name: #arg_ty_ident) -> &mut Self {
                    self.#field_name = Some(Some(#field_name));
                    self
                }
            }
        }
        FieldKind::RepeatedField {
            each_name: fn_name,
            arg_ty_ident,
        } =>
        {
            quote! {
                fn #fn_name(&mut self, #fn_name: #arg_ty_ident) -> &mut Self {
                    self.#field_name.get_or_insert_with(Vec::new).push(#fn_name);
                    self
                }
            }
        }
    };
    Ok(out)
}

enum FieldKind
{
    Field,
    OptionalField
    {
        arg_ty_ident: Ident,
    },
    RepeatedField
    {
        each_name:    Ident,
        arg_ty_ident: Ident,
    },
}

fn extract_field_type_kind(field: &Field) -> error::Result<FieldKind>
{
    let Type::Path(TypePath {
        attrs: _,
        qself: _,
        path: Path {
            leading_colon: _,
            segments: outer_segments,
        },
    }) = &field.ty
    else
    {
        return Err(BuilderMacroError::UnsupportedTypeKind(
            "We only expect path types here.",
        ));
    };

    let outer_segment = outer_segments
        .last()
        .ok_or(BuilderMacroError::UnsupportedTypeKind(
            "Bad types provided in struct.",
        ))?;

    if outer_segment.ident == "Option"
    {
        let inner_segment = get_inner_segment(outer_segment)?;

        Ok(FieldKind::OptionalField {
            arg_ty_ident: inner_segment.ident.clone(),
        })
    }
    else if outer_segment.ident == "Vec" && !field.attrs.is_empty()
    {
        let Some(attr) = field.attrs.first()
        else
        {
            return Err(error::BuilderMacroError::NoAttributeFound);
        };

        if attr.path().is_ident("builder")
        {
            let mut builder_method_name = String::new();
            attr.parse_nested_meta(|meta| {
                if meta.path.is_ident("each")
                {
                    builder_method_name = meta.value()?.parse::<syn::LitStr>()?.value();
                    Ok(())
                }
                else
                {
                    Err(meta.error("expected `builder(each = \"...\")`"))
                }
            })?;

            Ok(FieldKind::RepeatedField {
                each_name:    quote::format_ident!("{}", builder_method_name),
                arg_ty_ident: get_inner_segment(outer_segment)?.ident.clone(),
            })
        }
        else
        {
            Err(BuilderMacroError::FieldAttributesEmpty)
        }
    }
    else
    {
        Ok(FieldKind::Field)
    }
}

fn get_inner_segment(outer_segment: &syn::PathSegment) -> error::Result<syn::PathSegment>
{
    let PathArguments::AngleBracketed(angle_bracketed) = outer_segment.arguments.clone()
    else
    {
        return Err(BuilderMacroError::UnsupportedTypeKind(
            "We only expect angle bracketed types here!",
        ));
    };
    let Some(GenericArgument::Type(inner_type)) = angle_bracketed.args.first()
    else
    {
        return Err(BuilderMacroError::UnsupportedTypeKind(
            "We were kinda expecting a generic argument containing an inner type here.",
        ));
    };
    let Type::Path(TypePath {
        attrs: _,
        qself: _,
        path:
            Path {
                leading_colon: _,
                segments: inner_type_segments,
            },
    }) = inner_type
    else
    {
        return Err(BuilderMacroError::UnsupportedTypeKind(
            "We do not support types other than Path based types.",
        ));
    };
    let inner_segment = inner_type_segments
        .last()
        .ok_or(BuilderMacroError::UnsupportedTypeKind(
            "Bad types provided in struct.",
        ))?
        .clone();
    Ok(inner_segment)
}
