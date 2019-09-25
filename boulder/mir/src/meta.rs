use crate::{traits::UpdateStepIds, StepId};

#[derive(Debug, Clone)]
pub struct LastNeeded<T> {
    last: StepId,
    inner: T,
}

impl<T: UpdateStepIds> UpdateStepIds for LastNeeded<T> {
    fn update_step_ids(&mut self, f: &mut dyn FnMut(&mut StepId)) {
        f(&mut self.last);
        self.inner.update_step_ids(f);
    }
}
