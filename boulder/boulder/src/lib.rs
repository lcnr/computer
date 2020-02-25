#[cfg(feature = "profiler")]
#[macro_use]
extern crate thread_profiler;

use global_ctx::GlobalCtx;

use diagnostics::CompileError;

use mir::{LangItemState, Mir};

pub fn compile_to_mir<'a>(
    ctx: &'a GlobalCtx,
    src: &'a str,
    file: &'a str,
) -> Result<Mir<'a>, CompileError> {
    #[cfg(feature = "profiler")]
    profile_scope!("compile_to_mir");
    let hir = parse::parse(&ctx, src, file)?;
    let hir = hir.resolve_types()?;
    let hir = hir.resolve_identifiers()?;
    let hir = hir.resolve_expr_types()?;
    let mut mir = hir.into_mir()?;
    mir.validate();
    mir.kill_uninhabited();
    mir.validate();
    mir.remove_noop_extend();
    mir.validate();
    Ok(mir)
}

pub fn core_optimizations<'a>(mir: &mut Mir<'a>, lang_items: LangItemState) {
    #[cfg(feature = "profiler")]
    profile_scope!("core_optimizations");
    mir.unify_blocks();
    mir.validate();
    mir.remove_unused_steps();
    mir.validate();
    mir.remove_unused_functions(lang_items);
    mir.validate();
    mir.remove_redirects();
    mir.validate();
}

pub fn compile<'a>(
    ctx: &'a GlobalCtx,
    src: &'a str,
    file: &'a str,
) -> Result<Mir<'a>, CompileError> {
    #[cfg(feature = "profiler")]
    profile_scope!("compile");
    let mut mir = compile_to_mir(ctx, src, file)?;
    core_optimizations(&mut mir, LangItemState::Unresolved);
    mir.reduce_binops();
    core_optimizations(&mut mir, LangItemState::BinopResolved);
    mir.reduce_sum_types();
    mir.validate();
    core_optimizations(&mut mir, LangItemState::BinopResolved);
    mir.reduce_to_bytes();
    mir.validate();
    core_optimizations(&mut mir, LangItemState::ToBytesResolved);
    Ok(mir)
}
