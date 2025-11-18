//! Comment token types.

use parserc::syntax::Syntax;

use crate::input::CSTInput;

define_token!(SlashSlashSlash, "///");
define_token!(SlashSlashNot, "//!");
define_token!(SlashSlash, "//");
define_token!(SlashStar, "/*");
define_token!(SlashStarStar, "/**");
define_token!(SlashStarNot, "/*!");
define_token!(StarSlash, "*/");

/// Comment start token.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum CommentStart<I>
where
    I: CSTInput,
{
    SlashStarNot(SlashStarNot<I>),
    SlashStarStar(SlashStarStar<I>),
    SlashSlashSlash(SlashSlashSlash<I>),
    SlashSlashNot(SlashSlashNot<I>),
    SlashSlash(SlashSlash<I>),
    SlashStar(SlashStar<I>),
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_comment_start {
        ($ident: ident, $input: literal, $match: literal) => {
            assert_eq!(
                parserc::syntax::InputSyntaxExt::parse(&mut crate::input::TokenStream::from(
                    $input
                )),
                Ok(CommentStart::$ident($ident(
                    crate::input::TokenStream::from($match)
                )))
            )
        };
    }

    #[test]
    fn test_comment_token() {
        assert_comment_start!(SlashSlashSlash, "///-", "///");
        assert_comment_start!(SlashSlashNot, "//!!", "//!");
        assert_comment_start!(SlashSlash, "//-", "//");
        assert_comment_start!(SlashStar, "/*~", "/*");
        assert_token_parse!(StarSlash, "*//", "*/");
    }
}
