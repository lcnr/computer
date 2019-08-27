use diagnostics::CompileError;

pub fn compile(src: &str) -> Result<mir::Mir, CompileError> {
    let hir = parse::parse(&src)?;
    let hir = hir.resolve_types()?;
    let hir = hir.resolve_identifiers()?;
    let hir = hir.resolve_expr_types()?;
    let mut mir = hir.to_mir()?;
    mir::optimize::kill_unreachable(&mut mir);
    Ok(mir)
}
