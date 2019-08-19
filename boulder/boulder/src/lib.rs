use boulder_core::CompileError;
use hir::Hir;

pub fn compile(src: &str) -> Result<Hir, CompileError> {
    let hir = parse::parse(&src)?;

    hir.type_ck()?;

    Ok(hir)
}
