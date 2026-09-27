use proc_macro::TokenStream;
mod error;
use error::{
    CustomDebugMacroError,
    Result,
};
use quote::quote;
use syn::{
    Data,
    DeriveInput,
    Ident,
    parse_macro_input,
};

#[proc_macro_derive(CustomDebug)]
pub fn derive(input: TokenStream) -> TokenStream
{
    let DeriveInput {
        attrs: _,
        vis: _,
        ident,
        generics: _,
        data,
    } = parse_macro_input!(input as DeriveInput);
    let struct_name_string = ident.to_string();
    let (field_name_strings, field_name_idents) = extract_field_names(&data).unwrap();

    let output_tokens = quote! {
        impl std::fmt::Debug for #ident {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(#struct_name_string)
                #(
                    .field(#field_name_strings, &self.#field_name_idents)
                )*
                    .finish()
            }
        }
    };

    output_tokens.into()
}

fn extract_field_names(data: &Data) -> Result<(Vec<String>, Vec<Ident>)>
{
    match data
    {
        Data::Struct(data_struct) =>
        {
            match &data_struct.fields
            {
                syn::Fields::Named(fields_named) =>
                {
                    let idents = fields_named
                        .named
                        .iter()
                        .filter_map(|field| {
                            field.ident.clone().map(|ident| (ident.to_string(), ident))
                        })
                        .collect::<(Vec<String>, Vec<Ident>)>();
                    Ok(idents)
                }
                syn::Fields::Unnamed(_) => todo!(),
                syn::Fields::Unit => todo!(),
            }
        }
        Data::Enum(_) | Data::Union(_) => Err(CustomDebugMacroError::UnsupportedTypeItem),
    }
}
