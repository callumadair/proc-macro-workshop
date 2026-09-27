pub type Result<T> = core::result::Result<T, CustomDebugMacroError>;

#[derive(Debug, thiserror::Error)]
pub(crate) enum CustomDebugMacroError
{
    #[error("No ident found in field")]
    MissingFieldIdent,
    #[error("We only support an implementation on structs at this point.")]
    UnsupportedTypeItem,
}
