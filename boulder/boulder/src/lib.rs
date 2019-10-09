#[cfg(feature = "profiler")]
#[macro_use]
extern crate thread_profiler;

use diagnostics::CompileError;

use mir::Mir;

pub fn compile_to_mir(src: &str, file: &str) -> Result<Mir, CompileError> {
    #[cfg(feature = "profiler")]
    profile_scope!("compile_to_mir");
    let hir = parse::parse(src, file)?;
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
    #[cfg(feature = "profiler")]
    profile_scope!("core_optimizations");
    mir.unify_blocks();
    mir.validate();
    mir.remove_unused_steps();
    mir.validate();
    mir.remove_redirects();
    mir.validate();
}

pub fn compile(src: &str, file: &str) -> Result<Mir, CompileError> {
    #[cfg(feature = "profiler")]
    profile_scope!("compile");
    let mut mir = compile_to_mir(src, file)?;
    core_optimizations(&mut mir);
    mir.reduce_binops();
    mir.validate();
    Ok(mir)
}
