use super::*;

/// remove all steps after a step with an `Uninhabited` type
pub fn kill_unreachable(mir: &mut Mir) {
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
