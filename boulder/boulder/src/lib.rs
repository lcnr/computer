use diagnostics::CompileError;

use mir::Mir;

pub fn compile_to_mir(src: &str) -> Result<Mir, CompileError> {
    let hir = parse::parse(&src)?;
    let hir = hir.resolve_types()?;
    let hir = hir.resolve_identifiers()?;
    let hir = hir.resolve_expr_types()?;
    let mut mir = hir.to_mir()?;
    mir.validate();
    mir.kill_uninhabited();
    mir.validate();
    mir.remove_noop_extend();
    mir.validate();
    Ok(mir)
}

pub fn core_optimizations(mir: &mut Mir) {
    mir.unify_blocks();
    mir.validate();
    mir.remove_unused_steps();
    mir.validate();
    mir.remove_redirects();
    mir.validate();
}

pub fn compile(src: &str) -> Result<Mir, CompileError> {
    let mut mir = compile_to_mir(src)?;
    core_optimizations(&mut mir);
    mir.reduce_binops();
    mir.validate();
    println!("{}", mir);
    Ok(mir)
}
