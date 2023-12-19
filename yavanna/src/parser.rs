use crate::lexer::{Token, lex_yavanna_code};
use std::collections::VecDeque;

#[derive(Debug)]
enum AstNode {
    Function {
        name: String,
        params: Vec<Parameter>,
        return_type: Option<String>,
        body: Vec<AstNode>,
    },
    // TODO : other nodes
}

#[derive(Debug)]
struct Parameter {
    name: String,
    type_name: String,
}

struct Parser {
    tokens: VecDeque<Token>,
    current_token: Option<Token>,
}

impl Parser {
    fn new(input: &str) -> Self {
        let tokens = lex_yavanna_code(input);
        let current_token = tokens.front().cloned();
        Parser {
            tokens,
            current_token,
        }
    }

    fn parse(&mut self) -> Vec<AstNode> {
        let mut nodes = Vec::new();
        while self.current_token.is_some() {
            // TODO: Parsing logic
            if let Some(Token::Keyword(kw)) = &self.current_token {
                if kw == "func" {
                    nodes.push(self.parse_function());
                }
            }
            self.advance();
        }
        nodes
    }

    fn parse_function(&mut self) -> AstNode {
        // TODO
        AstNode::Function {
            name: String::new(),
            params: Vec::new(),
            return_type: None,
            body: Vec::new(),
        }
    }

    fn advance(&mut self) {
        self.tokens.pop_front();
        self.current_token = self.tokens.front().cloned();
    }

    // TODO
}

fn main() {
    let input = "In a hole in the ground ...";
    let mut parser = Parser::new(input);
    let ast = parser.parse();

    println!("{:#?}", ast);
}