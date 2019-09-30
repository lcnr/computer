use diagnostics::CompileError;

pub fn compile(src: &str) -> Result<mir::Mir, CompileError> {
    let hir = parse::parse(&src)?;
    let hir = hir.resolve_types()?;
    let hir = hir.resolve_identifiers()?;
    let hir = hir.resolve_expr_types()?;
    let mut mir = hir.to_mir()?;
    dbg!(mir.step_count());
    mir.validate();
    mir.kill_uninhabited();
    dbg!(mir.step_count());
    mir.validate();
    mir.remove_noop_extend();
    dbg!(mir.step_count());
    mir.validate();
    mir.reduce_binops();
    mir.validate();
    dbg!(mir.step_count());
    Ok(mir)
}
