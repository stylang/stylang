use crate::{expr::Expr, input::CSTInput};

/// A trait for priority-based combination between sub-expressions
pub(super) trait Composable<I>
where
    I: CSTInput,
{
    /// returns
    fn priority(&self) -> usize;

    /// Consumes itself and combines with another subexpression to create a new expression.
    fn compose<F>(self, priority: usize, f: F) -> Expr<I>
    where
        F: FnOnce(Expr<I>) -> Expr<I>;
}
