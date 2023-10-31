extern crate regex;

use regex::Regex;
use std::collections::VecDeque;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Keyword(String),
    Identifier(String),
    Number(String),
    StringLiteral(String),
    Operator(String),
    Punctuation(String),
    Whitespace(String),
    Comment(String),
}

pub fn lex_c_code(input: &str) -> VecDeque<Token> {
    let mut tokens = VecDeque::new();

    let token_patterns: &[(&str, fn(String) -> Token)] = &[
        (
            &[
                r"\bint\b",
                r"\breturn\b",
                r"\bif\b",
                r"\bwhile\b",
                r"\bfor\b",
                r"\bvoid\b",
                r"\bauto\b",
                r"\bbreak\b",
                r"\bcase\b",
                r"\bchar\b",
                r"\bconst\b",
                r"\bcontinue\b",
                r"\bdefault\b",
                r"\bdo\b",
                r"\bdouble\b",
                r"\belse\b",
                r"\benum\b",
                r"\bextern\b",
                r"\bfloat\b",
                r"\bgoto\b",
                r"\blong\b",
                r"\bregister\b",
                r"\bshort\b",
                r"\bsigned\b",
                r"\bsizeof\b",
                r"\bstatic\b",
                r"\bstruct\b",
                r"\bswitch\b",
                r"\btypedef\b",
                r"\bunion\b",
                r"\bunsigned\b",
                r"\bvolatile\b",
                r"\bwhile\b",
            ]
            .join("|"),
            |s| Token::Keyword(s),
        ),
        (r"[a-zA-Z_][a-zA-Z0-9_]*", |s| Token::Identifier(s)),
        (r"\d+", |s| Token::Number(s)),
        (r#"\"(\\.|[^"\\])*\""#, |s| Token::StringLiteral(s)),
        (r"\+|-|==|!=|>|<|>=|<=|&&|\|\||=", |s| Token::Operator(s)),
        // Can be multiplication or pointer dereference
        (r"\*", |s| Token::Operator(s)),
        // Can be address operator or bitwise AND
        (r"&", |s| Token::Operator(s)),
        (r"[\(\){};,]", |s| Token::Punctuation(s)),
        // Array brackets
        (r"\[|\]", |s| Token::Punctuation(s)),
        (r"[\s]+", |s| Token::Whitespace(s)),
        (r"//.*", |s| Token::Comment(s)),
    ];

    let mut remaining = input;

    while !remaining.is_empty() {
        let mut max_match: Option<(usize, fn(String) -> Token)> = None;

        for &(pattern, token_ctor) in token_patterns {
            let re = Regex::new(pattern).unwrap();
            if let Some(mat) = re.find(remaining) {
                // matches must start at current pos (sliding style)
                if mat.start() == 0 {
                    if max_match.is_none() || mat.end() > max_match.unwrap().0 {
                        max_match = Some((mat.end(), token_ctor));
                    }
                }
            }
        }

        match max_match {
            Some((end, ctor)) => {
                tokens.push_back(ctor(remaining[..end].to_string()));
                remaining = &remaining[end..];
            }
            None => panic!("Unexpected sequence: {}", remaining),
        }
    }

    tokens
}

#[cfg(test)]
mod lexer_tests {
    use super::*;

    #[test]
    fn test_keywords() {
        let input = "int void return";
        let tokens = lex_c_code(input);
        let expected = vec![
            Token::Keyword("int".to_string()),
            Token::Whitespace(" ".to_string()),
            Token::Keyword("void".to_string()),
            Token::Whitespace(" ".to_string()),
            Token::Keyword("return".to_string()),
        ];
        assert_eq!(tokens.into_iter().collect::<Vec<_>>(), expected);
    }

    #[test]
    fn test_identifiers() {
        let input = "variable anotherVar _private";
        let tokens = lex_c_code(input);
        let expected = vec![
            Token::Identifier("variable".to_string()),
            Token::Whitespace(" ".to_string()),
            Token::Identifier("anotherVar".to_string()),
            Token::Whitespace(" ".to_string()),
            Token::Identifier("_private".to_string()),
        ];
        assert_eq!(tokens.into_iter().collect::<Vec<_>>(), expected);
    }

    // #[test]
    // fn test_string_literals() {
    //     let input = r#""Hello" "World""#;
    //     let tokens = lex_c_code(input);
    //     let expected = vec![
    //         Token::StringLiteral("Hello".to_string()),
    //         Token::Whitespace(" ".to_string()),
    //         Token::StringLiteral("World".to_string())
    //     ];
    //     assert_eq!(tokens.into_iter().collect::<Vec<_>>(), expected);
    // }

    #[test]
    fn test_array_declaration() {
        let input = "int arr[10];";
        let tokens = lex_c_code(input);
        let expected = vec![
            Token::Keyword("int".to_string()),
            Token::Whitespace(" ".to_string()),
            Token::Identifier("arr".to_string()),
            Token::Punctuation("[".to_string()),
            Token::Number("10".to_string()),
            Token::Punctuation("]".to_string()),
            Token::Punctuation(";".to_string()),
        ];
        assert_eq!(tokens.into_iter().collect::<Vec<_>>(), expected);
    }
}
