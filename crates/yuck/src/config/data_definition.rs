use eww_shared_util::Span;

use crate::{
    ast_error::AstError,
    error::{DiagResult, DiagResultExt},
    parser::{ast::Ast, ast_iterator::AstIterator, from_ast::FromAstElementContent},
};

#[derive(Debug, Clone, PartialEq, Eq, serde::Deserialize)]
pub struct DataDefinition {
    pub name: String,
    pub type_vars: Vec<String>,
    pub conss: Vec<ConsDefinition>,
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Deserialize)]
pub struct ConsDefinition {
    name: String,
    args: Vec<String>,
}

impl FromAstElementContent for DataDefinition {
    const ELEMENT_NAME: &'static str = "data";

    fn from_tail<I: Iterator<Item = Ast>>(_span: Span, mut iter: AstIterator<I>) -> DiagResult<Self> {
        let result: DiagResult<_> = try {
            let (_name_span, name) = iter.expect_symbol()?;
            let (_args_span, args) = iter.expect_list()?;
            let type_vars = args.into_iter().map(Ast::as_symbol).collect::<Result<Vec<_>, _>>()?;

            let mut conss = Vec::new();

            while let Ok((_cons_span, cons)) = iter.expect_list() {
                let mut cons: Vec<String> = cons.into_iter().map(Ast::as_symbol).collect::<Result<Vec<_>, _>>()?;
                if cons.is_empty() {
                    Err(AstError::TooFewElements(_cons_span))?;
                }
                let name = cons.remove(0);
                let args = cons;
                conss.push(ConsDefinition { name, args })
            }

            iter.expect_done()?;

            Self { name, type_vars, conss }
        };
        result.note(r#"Expected format: `(data Either (a b) (Left a) (Right b))`"#)
    }
}
