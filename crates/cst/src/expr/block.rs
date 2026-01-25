use parserc::syntax::Syntax;

use crate::{
    attr::InnerAttribute,
    errors::{CSTError, SemanticsKind},
    expr::ExprWithoutBlock,
    input::CSTInput,
    lexical::{
        keywords::strict::{Async, Move},
        punct::{BraceEnd, BraceStart},
    },
    stmt::Stmt,
};

/// A block expression, or block, is a control flow expression and anonymous namespace scope
/// for items and variable declarations.
///
/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/expressions/block-expr.html
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(semantic = check_block_expr)]
pub struct BlockExpr<I>
where
    I: CSTInput,
{
    /// punct `{`
    pub delimiter_start: BraceStart<I>,
    /// inner attribute list.
    pub attrs: Vec<InnerAttribute<I>>,
    /// stmt list.
    pub stmts: Vec<Stmt<I>>,
    /// optional tailing expression
    pub tailing_expr: Option<ExprWithoutBlock<I>>,
    /// punct `}`
    pub delimiter_end: BraceEnd<I>,
}

#[inline]
fn check_block_expr<I>(_: I, block: BlockExpr<I>) -> Result<BlockExpr<I>, CSTError>
where
    I: CSTInput,
{
    if block.stmts.is_empty() && block.tailing_expr.is_none() {
        return Err(CSTError::Semantics(
            SemanticsKind::BlockExpr,
            block.to_span(),
        ));
    }

    Ok(block)
}

/// An async block is a variant of a block expression which evaluates to a future.
///
/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/expressions/block-expr.html#grammar-AsyncBlockExpression
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct AsyncBlockExpr<I>
where
    I: CSTInput,
{
    /// `async` keyword
    pub async_keyword: Async<I>,
    /// optional `move` keyword
    pub move_keyword: Option<Move<I>>,
    /// body of this async block expr.
    pub block: BlockExpr<I>,
}
