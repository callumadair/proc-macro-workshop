use proc_macro::TokenStream;

use quote::{format_ident, quote};
use syn::{Data, DeriveInput, parse_macro_input};

#[proc_macro_derive(Builder)]
pub fn derive(input: proc_macro::TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    let struct_ident = ast.ident;
    let builder_ident = format_ident!("{}Builder", struct_ident);
    let builder_error_ident = format_ident!("{}Error", builder_ident);

    let _input_data: Data = ast.data;
    let data = if let Data::Struct(data) = _input_data {
        data
    } else {
        unimplemented!()
    };

    let field_idents: Vec<proc_macro2::TokenStream> = data.fields.iter().map(|f| {
        let ident = &f.ident;
        quote! {
            #ident
        }
    }).collect();

    let field_types: Vec<proc_macro2::TokenStream> = data.fields.iter().map(|f| {
        let ty = &f.ty;
        quote! {
            #ty
        }
    }).collect();

    let struct_fields = quote! {
        #(#field_idents: Option<#field_types>,)*
    };

    let output = quote! {
        use std::{error::Error, fmt};

        #[derive(Debug, Clone)]
        struct DeriveError;

        impl Error for DeriveError {}

        impl fmt::Display for DeriveError {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, ": derivation error.")
            }
        }

        pub struct #builder_ident {
            #struct_fields
        }

        impl #struct_ident {
            pub fn builder() ->#builder_ident {
                #builder_ident {
                    #(#field_idents: None,)*
                }
            }
        }

        impl #builder_ident {

            pub fn build(&mut self) -> Result<#struct_ident, Box<dyn Error>> {
                #(if self.#field_idents.is_none() {
                    return Err(
                         Box::new(DeriveError {})
                     );
                })*

                Ok(#struct_ident {
                    #(#field_idents: self.#field_idents.clone().unwrap(),)*
                })
            }

            #(fn #field_idents(&mut self, #field_idents: #field_types) -> &mut Self {
                self.#field_idents = Some(#field_idents);
                self
            })*

        }
    };

    output.into()
}



