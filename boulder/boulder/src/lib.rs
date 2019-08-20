use diagnostics::{CompileError, Meta};
use hir::Hir;

pub fn compile(src: &str) -> Result<Hir<Meta<'_, hir::VariableId>, hir::TypeId>, CompileError> {
    let hir = parse::parse(&src)?;
    let hir = hir.resolve_types()?;
    Ok(hir)
}
