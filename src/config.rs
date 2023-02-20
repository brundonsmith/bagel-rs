use std::{collections::HashMap, path::PathBuf};

use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct BagelConfig {
    pub entry: String,

    pub platforms: Option<Vec<String>>,

    #[allow(non_snake_case)]
    pub checkerRuleOverrides: Option<HashMap<String, String>>,

    #[allow(non_snake_case)]
    pub remappedModules: Option<HashMap<String, String>>,
    // pub formatting_options
}
