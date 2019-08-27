use diagnostics::CompileError;

pub fn compile(src: &str) -> Result<Vec<u8>, CompileError> {
    let hir = parse::parse(&src)?;
    let hir = hir.resolve_types()?;
    let hir = hir.resolve_identifiers()?;
    let hir = hir.resolve_expr_types()?;
    let mir = hir.to_mir()?;
    let asm = mir.to_asm();
    let bytes = rock::codegen(&asm, &mut rock::DebugLogger).unwrap();
    Ok(bytes)
}
