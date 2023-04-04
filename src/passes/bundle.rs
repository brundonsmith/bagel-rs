use crate::model::module::ModulesStore;
use crate::passes::compile::CompileContext;
use std::fmt::Write;

impl ModulesStore {
    pub fn bundle<'a>(&self) -> String {
        let mut buf = String::new();

        // VERY NAIVE FOR NOW
        for module in self.iter().filter_map(|(_, module)| module.as_ref().ok()) {
            module.compile(
                CompileContext {
                    modules: self,
                    current_module: module,
                    include_types: false,
                },
                &mut buf,
            );
        }

        buf.write_str("\n\nmain();");

        buf
    }
}
