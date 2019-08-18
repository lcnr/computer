use boulder_core::CompileError;
use hir::HIR;

pub fn compile(src: &str) -> Result<HIR, CompileError> {
    let hir = parse::parse(&src)?;

    hir.type_ck()?;

    Ok(hir)
}
