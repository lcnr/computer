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
    /*{
        use mir::Object;
        let mut bmi = bmi::BoulderMirInterpreter::new(&mir);
        println!("{}", mir);
        for i in 0..2000 {
            println!("{}: {:?}", i, bmi.execute_function(0.into(), &vec![Object::U32(i)]).map(|v| {
                let b = if let Object::Variant(i, _) = v {
                    i == 2.into()
                } else {
                    false
                };

                fn is_prime(a: u32) -> bool {
                    if a < 2 {
                        return false;
                    }
                    for i in 2..a {
                        if a % i == 0 {
                            return false;
                        }
                    }
                    true
                }
                assert_eq!(is_prime(i), b);
                b
            }));
        }
    }*/
    mir.validate();
    dbg!(mir.step_count());
    Ok(mir)
}
