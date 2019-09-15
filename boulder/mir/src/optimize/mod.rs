use super::*;

/// remove all steps after a step with an `Uninhabited` type
/// this is always expected to run as the first optimization,
/// as all other optimizations expect only the last step
/// of a block to be a branch
pub fn kill_uninhabited(mir: &mut Mir) {
    let types = &mut mir.types;

    for func in mir.functions.iter_mut() {
        for block in func.content.iter_mut() {
            if let Some((pos, _)) = block
                .content
                .iter()
                .enumerate()
                .find(|(_, step)| types[step.ty.0] == Type::Uninhabited)
            {
                block.content.truncate(pos + 1);
            }
        }
    }
}
