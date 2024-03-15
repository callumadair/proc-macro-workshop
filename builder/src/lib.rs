use proc_macro2::{Ident, TokenStream};
use std::str::FromStr;

use quote::{format_ident, quote, ToTokens};
use syn::{parse_macro_input, Data, DataStruct, DeriveInput};

#[proc_macro_derive(Builder)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    let struct_ident = ast.ident;

    let _input_data: Data = ast.data;
    let data = if let Data::Struct(data) = _input_data {
        data
    } else {
        unimplemented!()
    };

    let output = generate_output(struct_ident, data);

    output.into()
}

fn generate_output(struct_ident: Ident, data: DataStruct) -> TokenStream {
    let builder_ident = format_ident!("{}Builder", struct_ident);
    let field_idents = generate_field_idents(&data);
    let field_types = generate_field_types(&data);
    let struct_fields = quote! {
        #(#field_idents: Option<#field_types>,)*
    };
    let builder_methods = generate_builder_methods(&data);

    let output: TokenStream = quote! {
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
                // TODO make this not throw an error if the field inside the option is itself an option.
                #(if self.#field_idents.is_none() {
                    return Err(
                         Box::new(DeriveError {})
                     );
                })*

                Ok(#struct_ident {
                    #(#field_idents: self.#field_idents.clone().unwrap(),)*
                })
            }

            #(#builder_methods)*
        }
    };
    output
}

fn generate_field_idents(data: &DataStruct) -> Vec<TokenStream> {
    let field_idents: Vec<TokenStream> = data
        .fields
        .iter()
        .map(|f| {
            let ident = &f.ident;
            quote! {
                #ident
            }
        })
        .collect();
    field_idents
}

fn generate_field_types(data: &DataStruct) -> Vec<TokenStream> {
    let field_types: Vec<TokenStream> = data
        .fields
        .iter()
        .map(|f| {
            let ty = &f.ty;
            quote! {
                #ty
            }
        })
        .collect();
    field_types
}

fn generate_builder_methods(data: &DataStruct) -> Vec<TokenStream> {
    let builder_methods: Vec<TokenStream> = data
        .fields
        .iter()
        .map(|f| {
            let field_ident = &f.ident;
            let field_type = &f.ty;

            let type_string = field_type.to_token_stream().to_string();
            if type_string.starts_with("Option < ") {
                let option_wrapped_type = &type_string[9..type_string.len() - 2];
                let field_type = TokenStream::from_str(option_wrapped_type).unwrap();
                quote! {
                    fn #field_ident(&mut self, #field_ident: #field_type) -> &mut Self {
                        self.#field_ident = Some(Some(#field_ident));
                        self
                    }
                }
            } else {
                quote! {
                    fn #field_ident(&mut self, #field_ident: #field_type) -> &mut Self {
                        self.#field_ident = Some(#field_ident);
                        self
                    }
                }
            }
        })
        .collect();
    builder_methods
}
