use std::{collections::{HashSet, HashMap}, mem, hash::Hash, cmp::Eq};

use diagnostics::{CompileError, Meta};

use crate::{
    function::VariableId,
    ty::{Type, TypeId},
};

