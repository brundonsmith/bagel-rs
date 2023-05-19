use std::collections::HashMap;

use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct BagelConfig {
    pub entry: String,

    pub platforms: Option<Vec<String>>,

    pub checkerRuleOverrides: Option<HashMap<String, String>>,

    pub remappedModules: Option<HashMap<String, String>>,
    // pub formatting_options
}
