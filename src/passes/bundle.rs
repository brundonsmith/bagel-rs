use crate::model::module::ModulesStore;
use crate::passes::compile::CompileContext;
use std::fmt::Write;

const CORE_TS: &str = include_str!("../../lib/ts/core.ts");
const REACTIVITY_TS: &str = include_str!("../../lib/ts/reactivity.ts");

impl ModulesStore {
    pub fn bundle<'a>(&self) -> String {
        let mut buf = String::new();

        buf += CORE_TS;
        buf += REACTIVITY_TS;

        let module_id_map = self
            .iter()
            .enumerate()
            .map(|(index, (module_id, _))| (module_id.clone(), index))
            .collect();

        // TODO: Sort by import graph
        // TODO: Helpful error about mutually imported consts

        // VERY NAIVE FOR NOW
        for module in self.iter().filter_map(|(_, module)| module.as_ref().ok()) {
            module.compile(
                CompileContext {
                    modules: self,
                    current_module: module,
                    include_types: false,
                    qualify_identifiers_with: Some(&module_id_map),
                    qualify_all_identifiers: false,
                },
                &mut buf,
            );
        }

        buf.write_str("\n\nmain();");

        buf
    }
}
