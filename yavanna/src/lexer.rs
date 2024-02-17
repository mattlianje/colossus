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
    Type(String),
}

pub fn lex_yavanna_code(input: &str) -> VecDeque<Token> {
    let mut tokens = VecDeque::new();

    let token_patterns: &[(&str, fn(String) -> Token)] = &[
        // Keywords
        (
            &[
                r"\bfunc\b",
                r"\breturn\b",
                r"\bif\b",
                r"\bwhile\b",
                r"\bfor\b",
                r"\belse\b",
            ]
            .join("|"),
            |s| Token::Keyword(s),
        ),
        // Type Keywords
        (r"\bint\b|\bfloat\b|\bstr\b", |s| Token::Type(s)),
        // Identifier
        (r"[a-zA-Z_][a-zA-Z0-9_]*", |s| Token::Identifier(s)),
        // Number
        (r"\d+", |s| Token::Number(s)),
        // String Literal
        (r#"\"(\\.|[^"\\])*\""#, |s| Token::StringLiteral(s)),
        // Operators
        (r"\+|-|==|!=|>|<|>=|<=|&&|\|\||=|\+=", |s| {
            Token::Operator(s)
        }),
        // Return type arrow
        (r"->", |s| Token::Operator(s)),
        // Punctuation
        (r"[\(\){};,]", |s| Token::Punctuation(s)),
        // Type annotation colon
        (r":", |s| Token::Punctuation(s)),
        // Whitespace
        (r"[\s]+", |s| Token::Whitespace(s)),
        // Single-line comment
        (r"//.*", |s| Token::Comment(s)),
        // Multi-line comment
        (r"/\*[\s\S]*?\*/", |s| Token::Comment(s)),
    ];

    let mut remaining = input;

    while !remaining.is_empty() {
        let mut max_match: Option<(usize, fn(String) -> Token)> = None;

        for &(pattern, token_ctor) in token_patterns {
            let re = Regex::new(pattern).unwrap();
            if let Some(mat) = re.find(remaining) {
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
        let input = "func return if";
        let tokens = lex_yavanna_code(input);
        let expected = vec![
            Token::Keyword("func".to_string()),
            Token::Whitespace(" ".to_string()),
            Token::Keyword("return".to_string()),
            Token::Whitespace(" ".to_string()),
            Token::Keyword("if".to_string()),
        ];
        assert_eq!(tokens.into_iter().collect::<Vec<_>>(), expected);
    }

    #[test]
    fn test_identifiers() {
        let input = "variable anotherVar _private";
        let tokens = lex_yavanna_code(input);
        let expected = vec![
            Token::Identifier("variable".to_string()),
            Token::Whitespace(" ".to_string()),
            Token::Identifier("anotherVar".to_string()),
            Token::Whitespace(" ".to_string()),
            Token::Identifier("_private".to_string()),
        ];
        assert_eq!(tokens.into_iter().collect::<Vec<_>>(), expected);
    }

    #[test]
    fn test_string_literals() {
        let input = r#""Hello" "World""#;
        let tokens = lex_yavanna_code(input);
        let expected = vec![
            Token::StringLiteral("\"Hello\"".to_string()),
            Token::Whitespace(" ".to_string()),
            Token::StringLiteral("\"World\"".to_string()),
        ];
        assert_eq!(tokens.into_iter().collect::<Vec<_>>(), expected);
    }

    #[test]
    fn test_function_declaration() {
        let input = "func someFunction(x: int) -> int { }";
        let tokens = lex_yavanna_code(input);
        let expected = vec![
            Token::Keyword("func".to_string()),
            Token::Whitespace(" ".to_string()),
            Token::Identifier("someFunction".to_string()),
            Token::Punctuation("(".to_string()),
            Token::Identifier("x".to_string()),
            Token::Punctuation(":".to_string()),
            Token::Whitespace(" ".to_string()),
            Token::Type("int".to_string()),
            Token::Punctuation(")".to_string()),
            Token::Whitespace(" ".to_string()),
            Token::Operator("->".to_string()),
            Token::Whitespace(" ".to_string()),
            Token::Type("int".to_string()),
            Token::Whitespace(" ".to_string()),
            Token::Punctuation("{".to_string()),
            Token::Whitespace(" ".to_string()),
            Token::Punctuation("}".to_string()),
        ];
        assert_eq!(tokens.into_iter().collect::<Vec<_>>(), expected);
    }
}
