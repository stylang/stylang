//! keyword tokens, more information see [`The Rust Reference`]
//!
//! [`The Rust Reference`]: https://doc.rust-lang.org/reference/keywords.html

macro_rules! define_keyword {
    ($ident: ident, $value: literal) => {
        #[doc = "define keyword `"]
        #[doc = stringify!($value)]
        #[doc = "`"]
        #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
        #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
        pub struct $ident<I>(pub I, pub Option<crate::lexical::S<I>>)
        where
            I: crate::input::CSTInput;

        impl<I> parserc::syntax::Syntax<I> for $ident<I>
        where
            I: crate::input::CSTInput,
        {
            #[inline]
            fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
                use parserc::Parser;
                use parserc::syntax::SyntaxInput;

                let keyword = parserc::keyword($value)
                    .parse(input)
                    .map_err(crate::errors::KeywordKind::$ident.map())?;

                if let Some(c) = input.iter().next() {
                    if c.is_alphanumeric() || c == '_' {
                        return Err(crate::errors::CSTError::Keyword(
                            crate::errors::KeywordKind::$ident,
                            parserc::ControlFlow::Recovable,
                            keyword.to_span(),
                        ));
                    }
                }

                Ok(Self(keyword, input.parse()?))
            }

            #[inline]
            fn to_span(&self) -> parserc::Span {
                self.0.to_span() + self.1.to_span()
            }
        }
    };
}

/// [`strict`](https://doc.rust-lang.org/reference/keywords.html#strict-keywords) keywords
pub mod strict {

    define_keyword!(As, "as");
    define_keyword!(Async, "async");
    define_keyword!(Await, "await");
    define_keyword!(Break, "break");
    define_keyword!(Const, "const");
    define_keyword!(Continue, "continue");
    define_keyword!(Crate, "crate");
    define_keyword!(Dyn, "dyn");
    define_keyword!(Else, "else");
    define_keyword!(Enum, "enum");
    define_keyword!(Extern, "extern");
    define_keyword!(False, "false");
    define_keyword!(Fn, "fn");
    define_keyword!(For, "for");
    define_keyword!(If, "if");
    define_keyword!(Impl, "impl");
    define_keyword!(In, "in");
    define_keyword!(Let, "let");
    define_keyword!(Loop, "loop");
    define_keyword!(Match, "match");
    define_keyword!(Mod, "mod");
    define_keyword!(Move, "move");
    define_keyword!(Mut, "mut");
    define_keyword!(Pub, "pub");
    define_keyword!(Ref, "ref");
    define_keyword!(Return, "return");
    define_keyword!(SelfLower, "self");
    define_keyword!(SelfUpper, "Self");
    define_keyword!(Static, "static");
    define_keyword!(Struct, "struct");
    define_keyword!(Super, "super");
    define_keyword!(Trait, "trait");
    define_keyword!(True, "true");
    define_keyword!(Type, "type");
    define_keyword!(Unsafe, "unsafe");
    define_keyword!(Use, "use");
    define_keyword!(Where, "where");
    define_keyword!(While, "while");
}

/// [`reserved`](https://doc.rust-lang.org/reference/keywords.html#reserved-keywords) keywords
pub mod reserved {

    define_keyword!(Abstract, "abstract");
    define_keyword!(Become, "become");
    define_keyword!(Box, "box");
    define_keyword!(Do, "do");
    define_keyword!(Final, "final");
    define_keyword!(Gen, "gen");
    define_keyword!(Macro, "macro");
    define_keyword!(Override, "override");
    define_keyword!(Priv, "priv");
    define_keyword!(Try, "try");
    define_keyword!(TypeOf, "typeof");
    define_keyword!(Unsized, "unsized");
    define_keyword!(Virtual, "virtual");
    define_keyword!(Yied, "yied");
}

/// [`weak`](https://doc.rust-lang.org/reference/keywords.html#weak-keywords) keywords
pub mod weak {
    define_keyword!(StaticLifetime, "'static");
    define_keyword!(MacroRules, "macro_rules");
    define_keyword!(Raw, "raw");
    define_keyword!(Safe, "safe");
    define_keyword!(Union, "union");
}

#[cfg(test)]
mod tests {
    use parserc::{ControlFlow, Span, syntax::SyntaxInput};

    use crate::{
        errors::{CSTError, KeywordKind},
        input::TokenStream,
        lexical::keywords::strict::{As, Async},
    };

    #[test]
    fn distinguish_between_as_and_async() {
        assert_eq!(
            TokenStream::from("async").parse::<Async<_>>(),
            Ok(Async(TokenStream::from("async"), None))
        );
        assert_eq!(
            TokenStream::from("as{").parse::<As<_>>(),
            Ok(As(TokenStream::from("as"), None))
        );

        assert_eq!(
            TokenStream::from("as_").parse::<As<_>>(),
            Err(CSTError::Keyword(
                KeywordKind::As,
                ControlFlow::Recovable,
                Span::Range(0..2)
            ))
        );

        assert_eq!(
            TokenStream::from("async").parse::<As<_>>(),
            Err(CSTError::Keyword(
                KeywordKind::As,
                ControlFlow::Recovable,
                Span::Range(0..2)
            ))
        );
    }
}
