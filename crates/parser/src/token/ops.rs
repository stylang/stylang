//! operator tokens.

use parserc::syntax::Syntax;

use crate::{OpKind, input::StylangInput};

/// Op `&&`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(keyword = "&&", map_err = OpKind::AndAnd.map())]
pub struct AndAnd<I>(pub I)
where
    I: StylangInput;

/// Op `&=`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(keyword = "&=", map_err = OpKind::AndEq.map())]
pub struct AndEq<I>(pub I)
where
    I: StylangInput;

/// Op `&`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(char = b'&', map_err = OpKind::And.map())]
pub struct And<I>(pub I)
where
    I: StylangInput;

/// Op `||`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(keyword = "||", map_err = OpKind::OrOr.map())]
pub struct OrOr<I>(pub I)
where
    I: StylangInput;

/// Op `|=`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(keyword = "|=", map_err = OpKind::OrEq.map())]
pub struct OrEq<I>(pub I)
where
    I: StylangInput;

/// Op `|`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(char = b'|', map_err = OpKind::Or.map())]
pub struct Or<I>(pub I)
where
    I: StylangInput;

/// Op `^=`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(keyword = "^=", map_err = OpKind::CaretEq.map())]
pub struct CaretEq<I>(pub I)
where
    I: StylangInput;

/// Op `^`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(char = b'^', map_err = OpKind::Caret.map())]
pub struct Caret<I>(pub I)
where
    I: StylangInput;

/// Op `->`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(keyword = "->", map_err = OpKind::RightArrow.map())]
pub struct RightArrow<I>(pub I)
where
    I: StylangInput;

/// Parser for `op` tokens.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Op<I>
where
    I: StylangInput,
{
    AndAnd(AndAnd<I>),
    AndEq(AndEq<I>),
    And(And<I>),
    OrOr(OrOr<I>),
    OrEq(OrEq<I>),
    Or(Or<I>),
    CaretEq(CaretEq<I>),
    Caret(Caret<I>),
    RightArrow(RightArrow<I>),
}

#[cfg(test)]
mod tests {
    use parserc::syntax::InputSyntaxExt;

    use super::*;
    use crate::input::TokenStream;

    #[test]
    fn test_ops() {
        macro_rules! make_test {
            ($ty: ident, $input: literal, $match: literal) => {
                (
                    TokenStream::from($input),
                    Op::$ty($ty(TokenStream::from($match))),
                )
            };
        }

        let tests = [
            make_test!(AndAnd, "&&==", "&&"),
            make_test!(AndEq, "&==", "&="),
            make_test!(And, "&a", "&"),
            make_test!(OrOr, "||===", "||"),
            make_test!(OrEq, "|===", "|="),
            make_test!(Or, "|a", "|"),
            make_test!(CaretEq, "^=", "^="),
            make_test!(Caret, "^|", "^"),
            make_test!(RightArrow, "->>", "->"),
        ];

        for (mut input, target) in tests {
            assert_eq!(input.parse(), Ok(target));
        }
    }
}
