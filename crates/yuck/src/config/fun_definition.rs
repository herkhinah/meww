use eww_shared_util::Span;
use simplexpr::SimplExpr;

use crate::{
    error::{DiagResult, DiagResultExt},
    parser::{ast::Ast, ast_iterator::AstIterator, from_ast::FromAstElementContent},
};

#[derive(Clone, Debug, PartialEq, Eq, serde::Serialize)]
pub struct FunDefinition {
    pub name: String,
    pub args: Vec<String>,
    pub body: Vec<SimplExpr>,
}

impl FromAstElementContent for FunDefinition {
    const ELEMENT_NAME: &'static str = "deffun";

    fn from_tail<I: Iterator<Item = Ast>>(_span: Span, mut iter: AstIterator<I>) -> DiagResult<Self> {
        let result: DiagResult<_> = try {
            let (_name_span, name) = iter.expect_symbol()?;

            let (_args_span, args) = iter.expect_list()?;
            let args = args.into_iter().map(Ast::as_symbol).collect::<Result<Vec<_>, _>>()?;

            let (_body_span, body) = iter.expect_list()?;
            let body = body.iter().map(Ast::as_simplexpr).collect::<Result<Vec<_>, _>>()?;

            iter.expect_done()?;
            Self { name, args, body }
        };
        result.note(r#"Expected format: `(deffun (arg0 arg1) (arg0 + arg1))`"#)
    }
}
