use quote::{
    ToTokens,
    quote,
};
use syn::{
    Attribute,
    Data,
    DeriveInput,
    GenericArgument,
    Ident,
    LitStr,
    Path,
    PathArguments,
    Type,
    TypePath,
    parse_macro_input,
};

use crate::error::BuilderMacroError::{
    self,
    UnsupportedTypeKind,
};

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
            #( #field_names: Option<#field_types>, )*
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
                                construct_builder_method(&field_ident, &field_ty).unwrap(),
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

fn construct_builder_method(
    field_name: &Ident,
    field_type: &Type,
) -> error::Result<proc_macro2::TokenStream>
{
    let out = match extract_field_type_kind(field_type)?
    {
        FieldKind::Field { arg_ty_ident: _ } =>
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
            fn_name,
            arg_ty_ident,
        } =>
        {
            quote! {
                fn #fn_name(&mut self, #field_name: #arg_ty_ident) -> &mut Self {
                    self.#field_name.push(#field_name);
                    self
                }
            }
        }
    };
    Ok(out)
}

enum FieldKind
{
    Field
    {
        arg_ty_ident: Ident
    },
    OptionalField
    {
        arg_ty_ident: Ident
    },
    RepeatedField
    {
        fn_name:      String,
        arg_ty_ident: Ident,
    },
}

fn extract_field_type_kind(ty: &Type) -> error::Result<FieldKind>
{
    let Type::Path(TypePath {
        attrs,
        qself: _,
        path: Path {
            leading_colon: _,
            segments: outer_segments,
        },
    }) = ty
    else
    {
        return Err(BuilderMacroError::UnsupportedTypeKind(
            "We only expect path types here.",
        ));
    };

    let Some(outer_segment) = outer_segments.last()
    else
    {
        return Err(BuilderMacroError::UnsupportedTypeKind(
            "Bad types provided in struct.",
        ));
    };

    if outer_segment.ident != "Option" && attrs.is_empty()
    {
        return Ok(FieldKind::Field {
            arg_ty_ident: outer_segment.ident.clone(),
        });
    }

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

    let Some(inner_segment) = inner_type_segments.last()
    else
    {
        return Err(BuilderMacroError::UnsupportedTypeKind(
            "Bad types provided in struct.",
        ));
    };

    if !attrs.is_empty()
    {
        let Some(attr) = attrs.first()
        else
        {
            return Err(error::BuilderMacroError::NoAttributeFound);
        };

        if attr.path().is_ident("builder")
        {
            // attr.parse_nested_meta(|meta| {
            //     if meta.path.is_ident("each")
            //     {
            //         builder_method_name = meta.value()?.parse()?;
            //         Ok(())
            //     }
            //     else
            //     {
            //         return Err("We don't care about this."));
            //     }
            // })?;
            

            Ok(FieldKind::RepeatedField {
                fn_name:      builder_method_name.value(),
                arg_ty_ident: inner_segment.ident.clone(),
            })
        }
        else
        {
            Err(BuilderMacroError::FieldAttributesEmpty)
        }
    }
    else
    {
        Ok(FieldKind::OptionalField {
            arg_ty_ident: inner_segment.ident.clone(),
        })
    }
}
