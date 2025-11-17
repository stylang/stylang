//! keyword tokens.

use parserc::syntax::Syntax;

use crate::{TokenKind, input::StylangInput};

/// keyword `fn`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(keyword = "fn", map_err = TokenKind::Fn.map())]
pub struct Fn<I>(I)
where
    I: StylangInput;

/// keyword `enum`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(keyword = "enum", map_err = TokenKind::Fn.map())]
pub struct Enum<I>(I)
where
    I: StylangInput;

/// keyword `struct`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(keyword = "struct", map_err = TokenKind::Fn.map())]
pub struct Struct<I>(I)
where
    I: StylangInput;

/// keyword `pub`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(keyword = "pub", map_err = TokenKind::Fn.map())]
pub struct Pub<I>(I)
where
    I: StylangInput;

/// keyword `native`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(keyword = "native", map_err = TokenKind::Fn.map())]
pub struct Native<I>(I)
where
    I: StylangInput;

/// keyword `use`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(keyword = "use", map_err = TokenKind::Fn.map())]
pub struct Use<I>(I)
where
    I: StylangInput;

/// keyword `mod`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(keyword = "mod", map_err = TokenKind::Fn.map())]
pub struct Mod<I>(I)
where
    I: StylangInput;

#[cfg(test)]
mod test {
    use super::*;
    use parserc::syntax::InputSyntaxExt;

    use crate::input::TokenStream;

    #[test]
    fn keyword() {
        macro_rules! make_test {
            ($ty: ident, $input: literal, $match: literal) => {
                assert_eq!(
                    TokenStream::from($input).parse(),
                    Ok($ty(TokenStream::from($match)))
                );
            };
        }

        make_test!(Fn, "fn12", "fn");
        make_test!(Enum, "enum12", "enum");
        make_test!(Struct, "struct12", "struct");

        make_test!(Pub, "pub12", "pub");
        make_test!(Native, "native123", "native");
        make_test!(Use, "use123", "use");
        make_test!(Mod, "mod123", "mod");
    }
}
