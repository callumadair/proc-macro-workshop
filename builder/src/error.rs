pub(crate) type Result<T> = core::result::Result<T, BuilderMacroError>;

#[derive(Debug, thiserror::Error)]
pub(crate) enum BuilderMacroError
{
    #[error("The field attributes are missing when we expect some.")]
    FieldAttributesEmpty,
    #[error("No ident found where expected: {0}")]
    MissingIdent(&'static str),
    #[error("No attribute found at position.")]
    NoAttributeFound,
    #[error("{0}")]
    SynError(#[from] syn::Error),
    #[error("We do not support this kind of type here: {0}")]
    UnsupportedTypeKind(&'static str),
}
