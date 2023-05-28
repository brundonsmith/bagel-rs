use crate::{
    model::{
        ast::{Expression, AST},
        Slice, SubsumationContext,
    },
    passes::{
        CheckContext, CompileContext, InferTypeContext, ResolveSymbolContext, ResolveTypeContext,
    },
};

impl<'a> From<InferTypeContext<'a>> for ResolveSymbolContext<'a> {
    fn from(
        InferTypeContext {
            modules,
            current_module,
            expressions_encountered: _,
        }: InferTypeContext<'a>,
    ) -> Self {
        Self {
            modules,
            current_module,
            follow_imports: true,
        }
    }
}

impl<'a> From<CompileContext<'a>> for ResolveSymbolContext<'a> {
    fn from(
        CompileContext {
            modules,
            current_module,
            include_types: _,
            qualify_identifiers_with: _,
            qualify_all_identifiers: _,
        }: CompileContext<'a>,
    ) -> Self {
        Self {
            modules,
            current_module,
            follow_imports: true,
        }
    }
}

impl<'a> From<SubsumationContext<'a>> for ResolveSymbolContext<'a> {
    fn from(
        SubsumationContext {
            modules,
            current_module,
            symbols_encountered: _,
        }: SubsumationContext<'a>,
    ) -> Self {
        Self {
            modules,
            current_module,
            follow_imports: true,
        }
    }
}

impl<'a> From<&CheckContext<'a>> for ResolveSymbolContext<'a> {
    fn from(
        CheckContext {
            modules,
            current_module,
            nearest_func_or_proc: _,
            in_expression_context: _,
        }: &CheckContext<'a>,
    ) -> Self {
        Self {
            modules,
            current_module,
            follow_imports: true,
        }
    }
}

impl<'a> From<&CheckContext<'a>> for SubsumationContext<'a> {
    fn from(
        CheckContext {
            modules,
            current_module,
            nearest_func_or_proc: _,
            in_expression_context: _,
        }: &CheckContext<'a>,
    ) -> Self {
        Self {
            modules,
            current_module,
            symbols_encountered: NO_SYMBOLS_ENCOUNTERED,
        }
    }
}

impl<'a> From<InferTypeContext<'a>> for SubsumationContext<'a> {
    fn from(
        InferTypeContext {
            modules,
            current_module,
            expressions_encountered: _,
        }: InferTypeContext<'a>,
    ) -> Self {
        Self {
            modules,
            current_module,
            symbols_encountered: NO_SYMBOLS_ENCOUNTERED,
        }
    }
}

impl<'a> From<ResolveSymbolContext<'a>> for SubsumationContext<'a> {
    fn from(
        ResolveSymbolContext {
            modules,
            current_module,
            follow_imports: _,
        }: ResolveSymbolContext<'a>,
    ) -> Self {
        Self {
            modules,
            current_module,
            symbols_encountered: NO_SYMBOLS_ENCOUNTERED,
        }
    }
}

const NO_SYMBOLS_ENCOUNTERED: &'static Vec<Slice> = &Vec::new();
const NO_EXPRESSIONS_ENCOUNTERED: &'static Vec<AST<Expression>> = &Vec::new();

impl<'a> From<InferTypeContext<'a>> for ResolveTypeContext<'a> {
    fn from(
        InferTypeContext {
            modules,
            current_module,
            expressions_encountered: _,
        }: InferTypeContext<'a>,
    ) -> Self {
        Self {
            modules,
            current_module,
        }
    }
}

impl<'a> From<&CheckContext<'a>> for ResolveTypeContext<'a> {
    fn from(
        CheckContext {
            modules,
            current_module,
            nearest_func_or_proc: _,
            in_expression_context: _,
        }: &CheckContext<'a>,
    ) -> Self {
        Self {
            modules,
            current_module,
        }
    }
}

impl<'a> From<SubsumationContext<'a>> for ResolveTypeContext<'a> {
    fn from(
        SubsumationContext {
            modules,
            current_module,
            symbols_encountered: _,
        }: SubsumationContext<'a>,
    ) -> Self {
        Self {
            modules,
            current_module,
        }
    }
}

impl<'a> From<&CheckContext<'a>> for InferTypeContext<'a> {
    fn from(
        CheckContext {
            modules,
            current_module,
            nearest_func_or_proc: _,
            in_expression_context: _,
        }: &CheckContext<'a>,
    ) -> Self {
        Self {
            modules,
            current_module,
            expressions_encountered: NO_EXPRESSIONS_ENCOUNTERED,
        }
    }
}

impl<'a> From<ResolveTypeContext<'a>> for InferTypeContext<'a> {
    fn from(
        ResolveTypeContext {
            modules,
            current_module,
        }: ResolveTypeContext<'a>,
    ) -> Self {
        Self {
            modules,
            current_module,
            expressions_encountered: NO_EXPRESSIONS_ENCOUNTERED,
        }
    }
}

impl<'a> From<CompileContext<'a>> for InferTypeContext<'a> {
    fn from(
        CompileContext {
            modules,
            current_module,
            include_types: _,
            qualify_identifiers_with: _,
            qualify_all_identifiers: _,
        }: CompileContext<'a>,
    ) -> Self {
        Self {
            modules,
            current_module,
            expressions_encountered: NO_EXPRESSIONS_ENCOUNTERED,
        }
    }
}
