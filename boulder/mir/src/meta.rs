use crate::{traits::UpdateStepIds, StepId};

pub trait LastNeeded {
    fn last_needed(&self) -> StepId;
}

#[derive(Debug, Clone)]
pub struct LastNeededStruct<T> {
    last: StepId,
    inner: T,
}

impl<T> LastNeeded for LastNeededStruct<T> {
    fn last_needed(&self) -> StepId {
        self.last
    }
}

impl<T: UpdateStepIds> UpdateStepIds for LastNeededStruct<T> {
    fn update_step_ids(&mut self, f: &mut dyn FnMut(&mut StepId)) {
        f(&mut self.last);
        self.inner.update_step_ids(f);
    }
}
