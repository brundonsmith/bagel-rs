use std::collections::HashMap;

use crate::model::{ModuleID, ParseError, ParsedModule};

pub type ModulesStore = HashMap<ModuleID, Result<ParsedModule, ParseError>>;
