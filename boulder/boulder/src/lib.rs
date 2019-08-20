use diagnostics::{CompileError, Meta};
use hir::Hir;

pub fn compile(
    src: &str,
) -> Result<Hir<Meta<'_, hir::VariableId>, hir::UnresolvedType>, CompileError> {
    let hir = parse::parse(&src)?;

    //hir.type_ck()?;

    Ok(hir)
}
