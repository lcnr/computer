#[cfg(feature = "profiler")]
#[macro_use]
extern crate thread_profiler;

use global_ctx::GlobalCtx;

use diagnostics::CompileError;

use mir::{LangItems, Mir};

use lir::Lir;

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

pub fn core<'a>(mir: &mut Mir<'a>, lang_items: LangItems) {
    #[cfg(feature = "profiler")]
    profile_scope!("core_optimizations");
    mir.simplify_single_match();
    mir.validate();
    mir.unify_blocks();
    mir.validate();
    mir.const_propagate();
    mir.validate();
    mir.remove_unused_steps();
    mir.validate();
    mir.remove_unused_functions(lang_items);
    mir.validate();
    mir.remove_redirects();
    mir.validate();
    mir.simplify_single_match();
    mir.validate();
}

/// All MIR optimizations used both during compiling and testing.
pub const MIR_OPTIMIZATIONS: &[(fn(&mut Mir), &str)] = &[
    (|mir| core(mir, LangItems::Unresolved), "core0"),
    (|mir| mir.reduce_binops(), "reduce_binops"),
    (|mir| core(mir, LangItems::BinopResolved), "core1"),
    // (|mir| println!("{}", mir), "print"),
    (|mir| mir.reduce_sum_types(), "reduce_sum_types"),
    // (|mir| println!("{}", mir), "print"),
    (|mir| core(mir, LangItems::BinopResolved), "core2"),
    (|mir| mir.reduce_to_bytes(), "reduce_to_bytes"),
    (|mir| core(mir, LangItems::ToBytesResolved), "core3"),
    (|mir| mir.enum_to_byte(), "enum_to_byte"),
    (|mir| core(mir, LangItems::ToBytesResolved), "core4"),
];

pub const LIR_OPTIMIZATIONS: &[(fn(&mut Lir), &str)] = &[
    (|lir| lir.remove_moves(), "remove_moves"),
    (|lir| lir.remove_dead_writes(), "dead_writes0"),
    (|lir| lir.loopify_tail_recursion(), "tail_recurse0"),
    (|lir| lir.remove_unused_functions(), "unused_functions0"),
    (|lir| lir.propagate_block_arguments(), "block_arguments0"),
    (|lir| lir.const_propagate(), "const_prop0"),
    (|lir| lir.merge_simple_blocks(), "merge_simple_blocks0"),
    (|lir| lir.remove_unused_blocks(), "unused_blocks0"),
    (|lir| lir.inline_functions(), "inline_functions0"),
    (|lir| lir.propagate_block_arguments(), "block_arguments1"),
    (|lir| lir.const_propagate(), "const_prop1"),
    (|lir| lir.remove_dead_writes(), "dead_writes1"),
    (|lir| lir.loopify_tail_recursion(), "tail_recurse1"),
    (|lir| lir.merge_simple_blocks(), "merge_simple_blocks1"),
    (|lir| lir.remove_unused_blocks(), "unused_blocks1"),
    (|lir| lir.remove_dead_writes(), "dead_writes2"),
    (|lir| lir.loopify_tail_recursion(), "tail_recurse2"),
    (|lir| lir.remove_unused_functions(), "unused_functions1"),
    (|lir| lir.inline_functions(), "inline_functions1"),
    (|lir| lir.remove_dead_writes(), "dead_writes3"),
    (|lir| lir.loopify_tail_recursion(), "tail_recurse3"),
    (|lir| lir.remove_unused_functions(), "unused_functions2"),
    (|lir| lir.propagate_block_arguments(), "block_arguments2"),
    (|lir| lir.const_propagate(), "const_prop2"),
    (|lir| lir.merge_simple_blocks(), "merge_simple_blocks2"),
    (|lir| lir.remove_unused_blocks(), "unused_blocks2"),
    (|lir| lir.remove_dead_writes(), "dead_writes4"),
    (|lir| lir.remove_unused_functions(), "unused_functions3"),
    (|lir| lir.minimize_memory_usage(), "final_minimize_mem"),
];

pub fn compile<'a>(
    ctx: &'a GlobalCtx,
    src: &'a str,
    file: &'a str,
) -> Result<String, CompileError> {
    #[cfg(feature = "profiler")]
    profile_scope!("compile");
    let mut mir = compile_to_mir(ctx, src, file)?;
    for (opt, _name) in MIR_OPTIMIZATIONS.iter() {
        opt(&mut mir);
        mir.validate();
    }

    let mut lir = mir2lir::convert(mir);
    lir.validate();
    for (opt, _name) in LIR_OPTIMIZATIONS.iter() {
        opt(&mut lir);
        lir.validate();
    }

    println!("{}", lir);
    let asm = lir2asm::convert(&lir);

    let data = rock::compile(&asm, &mut rock::DebugLogger).unwrap();
    let mut remu = remu::Remu::new();

    remu.memory_mut()[0..data.len()].copy_from_slice(&data);

    match remu.run(10_000_000) {
        Ok(steps) => println!("./{}: {}", file, steps),
        Err(e) => panic!("{:?}", e),
    }

    Ok(asm)
}
