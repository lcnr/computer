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
    mir.validate(false);
    mir.kill_uninhabited();
    mir.validate(false);
    mir.remove_noop_extend();
    mir.validate(false);
    Ok(mir)
}

pub fn core_optimizations<'a>(mir: &mut Mir<'a>, e2b: bool, lang_items: LangItemState) {
    #[cfg(feature = "profiler")]
    profile_scope!("core_optimizations");
    mir.simplify_single_match();
    mir.validate(e2b);
    mir.unify_blocks();
    mir.validate(e2b);
    mir.remove_unused_steps();
    mir.validate(e2b);
    mir.remove_unused_functions(lang_items);
    mir.validate(e2b);
    mir.remove_redirects();
    mir.validate(e2b);
    mir.simplify_single_match();
    mir.validate(e2b);
}

pub fn compile<'a>(
    ctx: &'a GlobalCtx,
    src: &'a str,
    file: &'a str,
) -> Result<Mir<'a>, CompileError> {
    #[cfg(feature = "profiler")]
    profile_scope!("compile");
    let mut mir = compile_to_mir(ctx, src, file)?;
    core_optimizations(&mut mir, false, LangItemState::Unresolved);
    mir.reduce_binops();
    core_optimizations(&mut mir, false, LangItemState::BinopResolved);
    mir.reduce_sum_types();
    mir.validate(false);
    core_optimizations(&mut mir, false, LangItemState::BinopResolved);
    mir.reduce_to_bytes();
    mir.validate(false);
    core_optimizations(&mut mir, false, LangItemState::ToBytesResolved);
    mir.enum_to_byte();
    mir.validate(true);
    core_optimizations(&mut mir, true, LangItemState::ToBytesResolved);
    Ok(mir)
}
