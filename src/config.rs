use std::{collections::HashMap, path::PathBuf};

use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct BagelConfig {
    pub main: String,
    pub platforms: Option<Vec<String>>,
    pub checker_rule_overrides: Option<HashMap<String, String>>,
    pub remapped_modules: Option<HashMap<String, String>>,
    // pub formatting_options
}
