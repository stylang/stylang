define_token_c!(BraceStart, b'{');
define_token_c!(BraceEnd, b'}');
define_token_c!(BracketStart, b'[');
define_token_c!(BracketEnd, b']');
define_token_c!(ParenStart, b'(');
define_token_c!(ParenEnd, b')');
define_token_c!(At, b'@');
define_token!(ArrowRight, "->");
define_token_c!(Colon, b':');
define_token_c!(Comma, b',');
define_token_c!(Semi, b';');
define_token!(PathSep, "::");

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_punct() {
        assert_token_parse!(BraceStart, "{=", "{");
        assert_token_parse!(BraceEnd, "}=", "}");
        assert_token_parse!(BracketStart, "[=", "[");
        assert_token_parse!(BracketEnd, "]=", "]");
        assert_token_parse!(ParenStart, "(=", "(");
        assert_token_parse!(ParenEnd, ")=", ")");
        assert_token_parse!(At, "@()", "@");
        assert_token_parse!(ArrowRight, "->>", "->");
        assert_token_parse!(Colon, ":=", ":");
        assert_token_parse!(Comma, ",=", ",");
        assert_token_parse!(Semi, ";;", ";");
        assert_token_parse!(PathSep, "::=", "::");
    }
}
