//! type parameters of stylang.

use parserc::{
    Parser,
    syntax::{Delimiter, Or, Punctuated, Syntax},
};

use crate::{
    errors::CSTError,
    expr::{Expr, ExprChain, ExprInfer, ExprLit, ExprUnary},
    input::CSTInput,
    keyword::{Const, Where},
    misc::Ident,
    path::Path,
    punct::{AngleBracketEnd, AngleBracketStart, Colon, Comma, Equal, Plus, Question},
    ty::Type,
};

/// A trait used as a bound on a type parameter.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct TraitBound<I>
where
    I: CSTInput,
{
    /// A modifier on a trait bound, currently only used for the ? in ?Sized.
    pub modifier: Option<Question<I>>,
    /// The Foo<&'a T> in for<'a> Foo<&'a T>
    pub path: Path<I>,
}

/// An individual generic argument, like  T, or Item = T.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum GenericArgument<I>
where
    I: CSTInput,
{
    /// A binding (equality constraint) on an associated type: the Item = u8 in Iterator<Item = u8>.
    Associated {
        ident: Ident<I>,
        eq: Equal<I>,
        ty: Or<Type<I>, ExprLit<I>>,
    },
    /// An associated type bound: Iterator<Item: Display>.
    Constraint {
        ident: Ident<I>,
        colon: Colon<I>,
        bounds: Punctuated<TraitBound<I>, Plus<I>>,
    },
    Const(#[parserc(parser = parse_const_expr)] Expr<I>),
    /// A type argument.
    Type(Type<I>),
}

#[inline]
fn parse_const_expr<I>(input: &mut I) -> Result<Expr<I>, CSTError>
where
    I: CSTInput,
{
    ExprChain::into_parser()
        .map(ExprChain::into)
        .or(ExprInfer::into_parser().map(Expr::Infer))
        .or(ExprUnary::into_parser().map(Expr::Unary))
        .parse(input)
}

/// A generic type parameter, or const generic: `T: Into<String>, 'a: 'b, const LEN: usize`.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum GenericParam<I>
where
    I: CSTInput,
{
    Const {
        #[parserc(crucial)]
        keyword: Const<I>,
        ident: Ident<I>,
        colon: Colon<I>,
        ty: Type<I>,
        default: Option<(Equal<I>, ExprLit<I>)>,
    },
    Type {
        ident: Ident<I>,
        bounds: Option<(Colon<I>, Punctuated<TraitBound<I>, Plus<I>>)>,
    },
}

/// A single predicate in a where clause: T: Deserialize<'de>.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct WherePredicate<I>
where
    I: CSTInput,
{
    pub ident: Ident<I>,
    pub colon: Colon<I>,
    pub bounds: Punctuated<TraitBound<I>, Plus<I>>,
}

/// A where clause in a definition: where T: Deserialize<'de>, D: 'static.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct WhereClause<I>
where
    I: CSTInput,
{
    /// leading keyword `where`
    pub keyword: Where<I>,
    /// predicate statments of where clause.
    pub predicates: Punctuated<WherePredicate<I>, Comma<I>>,
}

/// Type parameters attached to a declaration of a function, enum, trait, etc.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Generics<I>(
    pub Delimiter<AngleBracketStart<I>, AngleBracketEnd<I>, Punctuated<GenericParam<I>, Comma<I>>>,
)
where
    I: CSTInput;

#[cfg(test)]
mod test {

    use parserc::syntax::{SyntaxInput, Or, Punctuated};

    use crate::{
        expr::{Digits, LitNumber},
        input::TokenStream,
        misc::S,
        path::PathSegment,
    };

    use super::*;

    #[test]
    fn test_trait_bound() {
        assert_eq!(
            TokenStream::from("?Sizied").parse::<TraitBound<_>>(),
            Ok(TraitBound {
                modifier: Some(Question(None, TokenStream::from((0, "?")), None)),
                path: Path {
                    leading_pathsep: None,
                    segments: Punctuated {
                        pairs: vec![],
                        tail: Some(Box::new(PathSegment {
                            ident: Ident(TokenStream::from((1, "Sizied"))),
                            arguments: None
                        }))
                    }
                }
            })
        );
    }

    #[test]
    fn test_generic_argument() {
        assert_eq!(
            TokenStream::from("T = u8").parse::<GenericArgument<_>>(),
            Ok(GenericArgument::Associated {
                ident: Ident(TokenStream::from((0, "T"))),
                eq: Equal(
                    Some(S(TokenStream::from((1, " ")))),
                    TokenStream::from((2, "=")),
                    Some(S(TokenStream::from((3, " "))))
                ),
                ty: Or::First(Type::U8(TokenStream::from((4, "u8"))))
            })
        );

        assert_eq!(
            TokenStream::from("N = 19").parse::<GenericArgument<_>>(),
            Ok(GenericArgument::Associated {
                ident: Ident(TokenStream::from((0, "N"))),
                eq: Equal(
                    Some(S(TokenStream::from((1, " ")))),
                    TokenStream::from((2, "=")),
                    Some(S(TokenStream::from((3, " "))))
                ),
                ty: Or::Second(ExprLit::Number(LitNumber {
                    sign: None,
                    trunc: Some(Digits {
                        input: TokenStream::from((4, "19")),
                        value: 19
                    }),
                    fract: None,
                    exp: None
                }))
            })
        );

        assert_eq!(
            TokenStream::from("T: Display + Eq + ").parse::<GenericArgument<_>>(),
            Ok(GenericArgument::Constraint {
                ident: Ident(TokenStream::from((0, "T"))),
                colon: Colon(
                    None,
                    TokenStream::from((1, ":")),
                    Some(S(TokenStream::from((2, " "))))
                ),
                bounds: Punctuated {
                    pairs: vec![
                        (
                            TraitBound {
                                modifier: None,
                                path: Path {
                                    leading_pathsep: None,
                                    segments: Punctuated {
                                        pairs: vec![],
                                        tail: Some(Box::new(PathSegment {
                                            ident: Ident(TokenStream::from((3, "Display"))),
                                            arguments: None
                                        }))
                                    }
                                }
                            },
                            Plus(
                                Some(S(TokenStream::from((10, " ")))),
                                TokenStream::from((11, "+")),
                                Some(S(TokenStream::from((12, " "))))
                            )
                        ),
                        (
                            TraitBound {
                                modifier: None,
                                path: Path {
                                    leading_pathsep: None,
                                    segments: Punctuated {
                                        pairs: vec![],
                                        tail: Some(Box::new(PathSegment {
                                            ident: Ident(TokenStream::from((13, "Eq"))),
                                            arguments: None
                                        }))
                                    }
                                }
                            },
                            Plus(
                                Some(S(TokenStream::from((15, " ")))),
                                TokenStream::from((16, "+")),
                                Some(S(TokenStream::from((17, " "))))
                            )
                        )
                    ],
                    tail: None
                }
            })
        );
    }

    #[test]
    fn test_generics() {
        let expect = r##"{"start":[null,{"offset":0,"value":"<","_marker":null},null],"end":[null,{"offset":41,"value":">","_marker":null},null],"body":{"pairs":[[{"Const":{"keyword":[{"offset":1,"value":"const","_marker":null},{"offset":6,"value":" ","_marker":null}],"ident":{"offset":7,"value":"N","_marker":null},"colon":[null,{"offset":8,"value":":","_marker":null},{"offset":9,"value":" ","_marker":null}],"ty":{"U32":{"offset":10,"value":"u32","_marker":null}},"default":[[{"offset":13,"value":" ","_marker":null},{"offset":14,"value":"=","_marker":null},{"offset":15,"value":" ","_marker":null}],{"Number":{"sign":null,"trunc":{"input":{"offset":16,"value":"10","_marker":null},"value":10},"fract":null,"exp":null}}]}},[null,{"offset":18,"value":",","_marker":null},{"offset":19,"value":" ","_marker":null}]]],"tail":{"Type":{"ident":{"offset":20,"value":"T","_marker":null},"bounds":[[null,{"offset":21,"value":":","_marker":null},{"offset":22,"value":" ","_marker":null}],{"pairs":[[{"modifier":null,"path":{"leading_pathsep":null,"segments":{"pairs":[],"tail":{"ident":{"offset":23,"value":"Display","_marker":null},"arguments":null}}}},[{"offset":30,"value":" ","_marker":null},{"offset":31,"value":"+","_marker":null},{"offset":32,"value":" ","_marker":null}]],[{"modifier":null,"path":{"leading_pathsep":null,"segments":{"pairs":[],"tail":{"ident":{"offset":33,"value":"Debug","_marker":null},"arguments":null}}}},[{"offset":38,"value":" ","_marker":null},{"offset":39,"value":"+","_marker":null},{"offset":40,"value":" ","_marker":null}]]],"tail":null}]}}}}"##;

        assert_eq!(
            serde_json::from_str::<serde_json::Value>(expect).unwrap(),
            serde_json::to_value(
                &TokenStream::from("<const N: u32 = 10, T: Display + Debug + >")
                    .parse::<Generics<_>>()
                    .unwrap()
            )
            .unwrap()
        );
    }

    #[test]
    fn test_where_clause() {
        let expect = r##"{"keyword":[{"offset":0,"value":"where","_marker":null},{"offset":5,"value":"\t\n","_marker":null}],"predicates":{"pairs":[[{"ident":{"offset":7,"value":"T","_marker":null},"colon":[null,{"offset":8,"value":":","_marker":null},{"offset":9,"value":" ","_marker":null}],"bounds":{"pairs":[[{"modifier":null,"path":{"leading_pathsep":null,"segments":{"pairs":[],"tail":{"ident":{"offset":10,"value":"Display","_marker":null},"arguments":null}}}},[{"offset":17,"value":" ","_marker":null},{"offset":18,"value":"+","_marker":null},{"offset":19,"value":" ","_marker":null}]],[{"modifier":null,"path":{"leading_pathsep":null,"segments":{"pairs":[],"tail":{"ident":{"offset":20,"value":"Debug","_marker":null},"arguments":null}}}},[{"offset":25,"value":" ","_marker":null},{"offset":26,"value":"+","_marker":null},null]]],"tail":null}},[null,{"offset":27,"value":",","_marker":null},{"offset":28,"value":"\n ","_marker":null}]],[{"ident":{"offset":30,"value":"B","_marker":null},"colon":[null,{"offset":31,"value":":","_marker":null},{"offset":32,"value":" ","_marker":null}],"bounds":{"pairs":[],"tail":{"modifier":null,"path":{"leading_pathsep":null,"segments":{"pairs":[],"tail":{"ident":{"offset":33,"value":"Eq","_marker":null},"arguments":null}}}}}},[null,{"offset":35,"value":",","_marker":null},null]]],"tail":null}}"##;

        assert_eq!(
            serde_json::from_str::<serde_json::Value>(expect).unwrap(),
            serde_json::to_value(
                &TokenStream::from("where\t\nT: Display + Debug +,\n B: Eq,")
                    .parse::<WhereClause<_>>()
                    .unwrap()
            )
            .unwrap()
        );
    }

    #[test]
    fn test_where_clause_path() {
        let expect = r##"{"keyword":[{"offset":0,"value":"where","_marker":null},{"offset":5,"value":" ","_marker":null}],"predicates":{"pairs":[],"tail":{"ident":{"offset":6,"value":"T","_marker":null},"colon":[null,{"offset":7,"value":":","_marker":null},{"offset":8,"value":" ","_marker":null}],"bounds":{"pairs":[],"tail":{"modifier":null,"path":{"leading_pathsep":null,"segments":{"pairs":[[{"ident":{"offset":9,"value":"std","_marker":null},"arguments":null},[null,{"offset":12,"value":"::","_marker":null},null]],[{"ident":{"offset":14,"value":"iter","_marker":null},"arguments":null},[null,{"offset":18,"value":"::","_marker":null},null]]],"tail":{"ident":{"offset":20,"value":"Iterator","_marker":null},"arguments":{"leading_pathsep":null,"delimiter_start":[null,{"offset":28,"value":"<","_marker":null},null],"args":{"pairs":[],"tail":{"Associated":{"ident":{"offset":29,"value":"Item","_marker":null},"eq":[{"offset":33,"value":" ","_marker":null},{"offset":34,"value":"=","_marker":null},{"offset":35,"value":" ","_marker":null}],"ty":{"First":{"Path":{"leading_pathsep":null,"segments":{"pairs":[],"tail":{"ident":{"offset":36,"value":"char","_marker":null},"arguments":null}}}}}}}},"delimiter_end":[null,{"offset":40,"value":">","_marker":null},null]}}}}}}}}}"##;

        assert_eq!(
            serde_json::from_str::<serde_json::Value>(expect).unwrap(),
            serde_json::to_value(
                &TokenStream::from("where T: std::iter::Iterator<Item = char>")
                    .parse::<WhereClause<_>>()
                    .unwrap()
            )
            .unwrap()
        );
    }
}
