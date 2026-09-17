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
    Path,
    PathArguments,
    Type,
    TypePath,
    parse_macro_input,
};

#[proc_macro_derive(Builder)]
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
    let (field_names, field_types, builder_methods) = create_builder_fields_and_methods(data);
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

fn create_builder_fields_and_methods(
    data: Data
) -> (
    Vec<proc_macro2::TokenStream>,
    Vec<proc_macro2::TokenStream>,
    Vec<proc_macro2::TokenStream>,
)
{
    match data
    {
        Data::Struct(data_struct) =>
        {
            match data_struct.fields
            {
                syn::Fields::Named(fields_named) =>
                {
                    fields_named
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
                                construct_builder_method(&field_ident, &field_ty),
                            )
                        })
                        .collect::<(
                            Vec<proc_macro2::TokenStream>,
                            Vec<proc_macro2::TokenStream>,
                            Vec<proc_macro2::TokenStream>,
                        )>()
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
    field_ident: &Ident,
    field_type: &Type,
) -> proc_macro2::TokenStream
{
    match extract_field_type_kind(field_type)
    {
        FieldKind::Field(ident) =>
        {
            quote! {
                fn #field_ident(&mut self, #field_ident: #ident) -> &mut Self {
                    self.#field_ident = Some(#field_ident);
                    self
                }
            }
        }
        FieldKind::OptionalField(ident) =>
        {
            quote! {
                fn #field_ident(&mut self, #field_ident: #ident) -> &mut Self {
                    self.#field_ident = Some(Some(#field_ident));
                    self
                }
            }
        }
    }
}

enum FieldKind
{
    Field(Ident),
    OptionalField(Ident),
}

fn extract_field_type_kind(ty: &Type) -> FieldKind
{
    let Type::Path(TypePath {
        attrs: _,
        qself: _,
        path: Path {
            leading_colon: _,
            segments: outer_segments,
        },
    }) = ty
    else
    {
        unimplemented!("We do not support types other than Path based types.")
    };

    let Some(outer_segment) = outer_segments.last()
    else
    {
        unimplemented!("Bad types provided in struct.")
    };

    if outer_segment.ident != "Option"
    {
        return FieldKind::Field(outer_segment.ident.clone());
    }

    let PathArguments::AngleBracketed(angle_bracketed) = outer_segment.arguments.clone()
    else
    {
        unimplemented!("We don't do this case currently.");
    };
    let Some(GenericArgument::Type(inner_type)) = angle_bracketed.args.first()
    else
    {
        unimplemented!("We were kinda expecting a generic argument containing an inner type here.")
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
        unimplemented!("We do not support types other than Path based types.")
    };

    let Some(inner_segment) = inner_type_segments.last()
    else
    {
        unimplemented!("Bad types provided in struct.")
    };
    FieldKind::OptionalField(inner_segment.ident.clone())
}
