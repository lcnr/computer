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
    mir.unify_blocks();
    dbg!(mir.step_count());
    mir.validate();
    mir.remove_unused_steps();
    dbg!(mir.step_count());
    mir.validate();
    mir.remove_redirects();
    dbg!(mir.step_count());
    mir.validate();
    //mir.reduce_binops();
    dbg!(mir.step_count());
    mir.validate();
    if false {
        use mir::Object;
        let mut bmi = bmi::BoulderMirInterpreter::new(&mir);
        println!("{}", mir);
        println!(
            "{:?}",
            bmi.execute_function(0.into(), &[Object::U32(39), Object::U32(4)])
        );
    };

    Ok(mir)
}
