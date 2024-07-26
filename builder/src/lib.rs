use quote::{format_ident, quote};
use syn::{parse_macro_input, Data, DeriveInput};

/// This macro implements the builder pattern based off how std::Command uses the builder pattern.
#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let struct_ident = &ast.ident;
    let builder_ident = format_ident!("{}Builder", struct_ident);

    let _input_data: Data = ast.data;
    let data = if let Data::Struct(data) = _input_data {
        data
    } else {
        unimplemented!()
    };

    let builder_fields = data.fields.iter().map(|f| {
        let field_name = &f.ident;
        let original_ty = &f.ty;

        if ty_wrapped_type("Option", original_ty).is_some() {
            quote! { #field_name: #original_ty }
        } else {
            quote! { #field_name: std::option::Option<#original_ty>
            }
        }
    });

    let builder_methods = data.fields.iter().map(|f| {
        let field_name = f.ident.as_ref().unwrap();
        let original_ty = &f.ty;

        let (arg_type, value) =
            if let Some(inner_ty) = ty_wrapped_type("Option", original_ty) {
                (inner_ty, quote! { std::option::Option::Some(#field_name) })
            } else if
            builder_of(&f).is_some() {
                (original_ty, quote! { #field_name })
            } else {
                (original_ty, quote! { std::option::Option::Some(#field_name) })
            };
        let set_method = quote! {
            pub fn #field_name(&mut self, #field_name: #arg_type) -> &mut Self {
                self.#field_name = #value;
                self
            }
        };

        match extend_method(&f) {
            None => set_method.into(),
            Some((true, extend_method)) => extend_method,
            Some((false, extend_method)) => {
                // safe to generate both!
                let expr = quote! {
                    #set_method
                    #extend_method
                };
                expr.into()
            }
        }
    });

    let build_fields = data.fields.iter().map(|f| {
        let field_name = &f.ident;
        if ty_wrapped_type("Option", &f.ty).is_some() {
            quote! {
                // Why clone?
                #field_name: self.#field_name.clone()
            }
        } else {
            quote! {
                // Why clone?
                #field_name: self.#field_name.clone().ok_or(concat!(stringify!(#field_name), " is not set."))?
            }
        }
    });

    let build_empty = data.fields.iter().map(|f| {
        let field_name = &f.ident;

        quote! {
            #field_name: std::option::Option::None
        }
    });

    let doc = format!("\
            Implements the [builder pattern] for [`{}`].\n\
            \n\
            [builder pattern]: https://rust-lang-nursery.github.io/api-guidelines/type-safety.html#c-builder", struct_ident);

    let output = quote! {
        #[doc = #doc]
        pub struct #builder_ident {
            #(#builder_fields),*
        }

        impl #builder_ident {
            #(#builder_methods)*

            pub fn build(&self) -> std::result::Result<#struct_ident, std::boxed::Box<dyn: std::error::Error>> {
                std::result::Result::Ok(#struct_ident {
                    #(#build_fields,)*
                })
            }
        }
        impl #struct_ident {
            pub fn builder() -> #builder_ident {
                #builder_ident {
                    #(#build_empty,)*
                }
            }
        }
    };

    output.into()
}

// Taken from here: https://github.com/jonhoo/proc-macro-workshop/blob/master/builder/src/lib.rs
// This mostly makes sense to me, but some bits are kinda tricky.
// Figure out how this all works exactly, read the appropriate syn docs and then use this or something like this
// to make it more concise like: https://github.com/jonhoo/proc-macro-workshop/blob/master/builder/src/lib.rs?#L25-L33
fn ty_wrapped_type<'a>(wrapper: &str, ty: &'a syn::Type) -> Option<&'a syn::Type> {
    if let syn::Type::Path(ref type_path) = ty {
        // If there is more than one thing wrapped inside the path? wrapper OR the wrapper is not the first segments ident return None.
        if type_path.path.segments.len() != 1 || type_path.path.segments[0].ident != wrapper {
            return None;
        }

        // If there is an angle bracketed generic type do below:
        if let syn::PathArguments::AngleBracketed(ref inner_ty) =
            type_path.path.segments[0].arguments
        {
            // If there is not only one type wrapped in the generic return None?
            if inner_ty.args.len() != 1 {
                return None;
            }

            // Convert the wrapped type into a usable enum.
            let inner_ty = inner_ty.args.first().unwrap();
            // Get the type of the generic argument.
            if let syn::GenericArgument::Type(ref t) = inner_ty {
                return Some(t);
            }
        }
    }
    // Return None if there isn't a valid type path.
    None
}

//TODO figure out how this works
fn builder_of(f: &syn::Field) -> Option<&syn::Attribute> {
    for attr in &f.attrs {
        if attr.path().segments.len() == 1 && attr.path().segments[0].ident == "builder" {
            return Some(attr);
        }
    }
    None
}

//TODO figure out how this works.
fn extend_method(f: &syn::Field) -> Option<(bool, proc_macro2::TokenStream)> {
    let name = f.ident.as_ref().unwrap();
    let g = builder_of(f)?;

    fn mk_err<T: quote::ToTokens>(t: T) -> Option<(bool, proc_macro2::TokenStream)> {
        Some((
            false,
            syn::Error::new_spanned(t, "expected `builder(each = \"...\")`").to_compile_error(),
        ))
    }

    let mesa = match g.parse_meta() {
        Ok(syn::Meta::List(mut nvs)) => {
            // list here is .. in #[builder(..)]
            // TODO find the equivalent in syn 2.0+
            assert_eq!(nvs.ident, "builder");
            if nvs.nested.len() != 1 {
                return mk_err(nvs);
            }

            // nvs.nested[0] here is (hopefully): each = "foo"
            // TODO find the equivalent in syn 2.0+
            match nvs.nested.pop().unwrap().into_value() {
                // TODO find the equivalent in syn 2.0+
                syn::jhoNestedMeta::Meta(syn::Meta::NameValue(nv)) => {
                    if nv.ident != "each" {
                        return mk_err(nvs);
                    }
                    nv
                }
                meta => {
                    // nvs.nested[0] was not k = v
                    return mk_err(meta);
                }
            }
        }
        Ok(meta) => {
            // inside of #[] there was either just an identifier (`#[builder]`) or a key-value
            // mapping (`#[builder = "foo"]`), neither of which are okay.
            return mk_err(meta);
        }
        Err(e) => {
            return Some((false, e.to_compile_error()));
        }
    };

    match meta.lit {
        syn::Lit::Str(s) => {
            let arg = syn::Ident::new(&s.value(), s.span());
            let inner_ty = ty_wrapped_type("Vec", &f.ty).unwrap();
            let method = quote! {
                pub fn #arg(&mut self, #arg: #inner_ty) -> &mut Self {
                    self.#name.push(#arg);
                    self
                }
            };
            Some((&arg == name, method))
        }
        lit => panic!("expected string, found {:?}", lit),
    }
}