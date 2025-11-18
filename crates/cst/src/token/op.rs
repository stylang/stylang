use crate::input::CSTInput;

define_token_c!(And, b'&');
define_token!(AndAnd, "&&");
define_token!(AndEq, "&=");
define_token_c!(Or, b'|');
define_token!(OrOr, "||");
define_token!(OrEq, "|=");
define_token_c!(Caret, b'^');
define_token!(CaretEq, "^=");
define_token_c!(Percent, b'%');
define_token!(PercentEq, "%=");
define_token_c!(Slash, b'/');
define_token!(SlashEq, "/=");
define_token_c!(Star, b'*');
define_token!(StarStar, "**");
define_token!(StarEq, "*=");
define_token_c!(Plus, b'+');
define_token!(PlusEq, "+=");
define_token_c!(Minus, b'-');
define_token!(MinusEq, "-=");
define_token!(Shr, ">>");
define_token!(ShrEq, ">>=");
define_token!(Shl, "<<");
define_token!(ShlEq, "<<=");
define_token_c!(Eq, b'=');
define_token!(EqEq, "==");
define_token!(FatArrowRight, "=>");
define_token_c!(Gt, b'>');
define_token!(Ge, ">=");
define_token_c!(Lt, b'<');
define_token!(Le, "<=");
define_token_c!(Not, b'!');
define_token!(NotEq, "!=");
define_token!(Sg, "/>");
define_token!(Ls, "</");

/// A parser for operator tokens.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, parserc::syntax::Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Operator<I>
where
    I: CSTInput,
{
    AndAnd(AndAnd<I>),
    AndEq(AndEq<I>),
    And(And<I>),
    OrOr(OrOr<I>),
    OrEq(OrEq<I>),
    Or(Or<I>),
    CaretEq(CaretEq<I>),
    Caret(Caret<I>),
    PercentEq(PercentEq<I>),
    Percent(Percent<I>),
    SlashEq(SlashEq<I>),
    Sg(Sg<I>),
    Slash(Slash<I>),
    StarStar(StarStar<I>),
    StarEq(StarEq<I>),
    Star(Star<I>),
    PlusEq(PlusEq<I>),
    Plus(Plus<I>),
    MinusEq(MinusEq<I>),
    Minus(Minus<I>),
    ShrEq(ShrEq<I>),
    Shr(Shr<I>),
    ShlEq(ShlEq<I>),
    Shl(Shl<I>),
    Ge(Ge<I>),
    Gt(Gt<I>),
    Le(Le<I>),
    Ls(Ls<I>),
    Lt(Lt<I>),
    NotEq(NotEq<I>),
    Not(Not<I>),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_op() {
        macro_rules! assert_op {
            ($ident: ident, $input: literal, $match: literal) => {
                assert_eq!(
                    parserc::syntax::InputSyntaxExt::parse(&mut crate::input::TokenStream::from(
                        $input
                    )),
                    Ok(Operator::$ident($ident(crate::input::TokenStream::from(
                        $match
                    ))))
                )
            };
        }

        assert_op!(And, "&,", "&");
        assert_op!(AndAnd, "&&", "&&");
        assert_op!(AndEq, "&==", "&=");
        assert_op!(Or, "|^", "|");
        assert_op!(OrOr, "||=", "||");
        assert_op!(OrEq, "|==", "|=");
        assert_op!(Caret, "^^", "^");
        assert_op!(CaretEq, "^==", "^=");
        assert_op!(Percent, "%^", "%");
        assert_op!(PercentEq, "%==", "%=");
        assert_op!(Slash, "/^", "/");
        assert_op!(SlashEq, "/==", "/=");
        assert_op!(Star, "*^", "*");
        assert_op!(StarStar, "**/", "**");
        assert_op!(StarEq, "*==", "*=");
        assert_op!(PlusEq, "+=", "+=");
        assert_op!(Plus, "++", "+");
        assert_op!(MinusEq, "-=", "-=");
        assert_op!(Minus, "--", "-");
        assert_op!(ShlEq, "<<==", "<<=");
        assert_op!(Shl, "<<<", "<<");
        assert_op!(ShrEq, ">>==", ">>=");
        assert_op!(Shr, ">>|", ">>");
        assert_op!(Ge, ">==", ">=");
        assert_op!(Gt, ">", ">");
        assert_op!(Le, "<=^", "<=");
        assert_op!(Lt, "<|", "<");
        assert_op!(NotEq, "!==", "!=");
        assert_op!(Not, "!!", "!");
        assert_op!(Sg, "/>>", "/>");
        assert_op!(Ls, "</==", "</");
    }
}
