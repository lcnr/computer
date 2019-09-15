use crate::ty::TypeId;

pub fn initialized_mir_block(
    id: mir::BlockId,
    variables: &[TypeId],
    var_lookup: &mut [Option<mir::StepId>],
    func: &mut mir::Function,
) {
    let block = func.block(id);
    for (i, var) in var_lookup
        .iter_mut()
        .enumerate()
        .filter_map(|(i, v)| v.as_mut().map(|v| (i, v)))
    {
        *var = block.add_input(variables[i].to_mir());
    }
}
