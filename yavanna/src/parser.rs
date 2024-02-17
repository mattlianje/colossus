use crate::lexer::{lex_yavanna_code, Token};
use std::collections::VecDeque;

fn main() {
    let input = "your input string here";
    let tokens = lex_yavanna_code(input);
    let mut parser = Parser::new(tokens);
    let ast = parser.parse();
    println!("{:#?}", ast);
}

#[derive(Debug)]
enum AstNode {
    Function {
        name: String,
        params: Vec<Parameter>,
        return_type: Option<String>,
        body: Vec<AstNode>,
    },
    VariableDeclaration {
        var_type: String,
        identifier: String,
        value: Option<Box<AstNode>>,
    },
    IfStatement {
        condition: Box<AstNode>,
        body: Vec<AstNode>,
        else_body: Option<Vec<AstNode>>,
    },
    WhileStatement {
        condition: Box<AstNode>,
        body: Vec<AstNode>,
    },
    ForStatement {
        initialization: Box<AstNode>,
        condition: Box<AstNode>,
        update: Box<AstNode>,
        body: Vec<AstNode>,
    },
    ExpressionStatement {
        expression: Box<AstNode>,
    },
    Assignment {
        identifier: String,
        value: Box<AstNode>,
    },
    BinaryOperation {
        operator: BinaryOperator,
        left: Box<AstNode>,
        right: Box<AstNode>,
    },
    Literal {
        value: LiteralValue,
    },
    Identifier(String),
    // TODO: Check this
}

#[derive(Debug)]
enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Equal,
    NotEqual,
    GreaterThan,
    LessThan,
    GreaterThanOrEqual,
    LessThanOrEqual,
}

#[derive(Debug)]
enum LiteralValue {
    Number(String),
    StringLiteral(String),
    // TODO : Finish this
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

// Note: Using Box to heap allocate for potentially deeply nested trees
// but will need to check if this is civilized Rust.
impl Parser {
    // Create parser and always start with first token
    pub fn new(tokens: VecDeque<Token>) -> Self {
        let current_token = tokens.front().cloned();
        Parser {
            tokens,
            current_token,
        }
    }

    pub fn parse(&mut self) -> Vec<AstNode> {
        let mut nodes = Vec::new();
        while let Some(token) = &self.current_token {
            match token {
                Token::Keyword(kw) => match kw.as_str() {
                    "func" => nodes.push(self.parse_function()),
                    "if" => nodes.push(self.parse_if_statement()),
                    "while" => nodes.push(self.parse_while_statement()),
                    "for" => nodes.push(self.parse_for_statement()),
                    // handle else with if statement / parse_if_statement

                    // TODO advancing past unrecognized keywords but these
                    // should be handled properly
                    _ => self.advance(),
                },
                Token::Identifier(_) => nodes.push(self.parse_identifier_usage()),
                Token::Type(_) => nodes.push(self.parse_type_related()),
                Token::Operator(_) => nodes.push(self.parse_expression()),
                Token::Number(_) | Token::StringLiteral(_) => nodes.push(self.parse_literal()),
                Token::Punctuation(punct) => match punct.as_str() {
                    // TODO: come back to this
                    // For now and for simplicity I am handling {}and[]
                    // in their other parsing counterparts
                    _ => self.advance(),
                },
                // Check in the dragon boook if these should be ignored competely
                Token::Whitespace(_) | Token::Comment(_) => self.advance(),
                _ => self.advance(),
            }
        }
        nodes
    }

    fn parse_identifier_usage(&mut self) -> AstNode {
        let identifier = self.expect_identifier();
        AstNode::Identifier(identifier)
    }

    fn parse_type_related(&mut self) -> AstNode {
        let type_str = self.expect_type();
        AstNode::Identifier(type_str)
    }

    fn parse_function(&mut self) -> AstNode {
        self.expect_keyword("func");
        let name = self.expect_identifier();
        self.expect_punctuation("(");
        let params = if !self.current_token_is_punctuation(")") {
            self.parse_parameter_list()
        } else {
            Vec::new()
        };
        self.expect_punctuation(")");
        let return_type = if self.current_token_is_operator("->") {
            self.advance();
            Some(self.expect_type())
        } else {
            None
        };
        let body = self.parse_compound_statement();

        AstNode::Function {
            name,
            params,
            return_type,
            body,
        }
    }

    fn parse_parameter_list(&mut self) -> Vec<Parameter> {
        let mut params = Vec::new();
        loop {
            params.push(self.parse_parameter());
            if !self.current_token_is_punctuation(",") {
                break;
            }
            self.advance();
        }
        params
    }

    fn parse_parameter(&mut self) -> Parameter {
        let name = self.expect_identifier();
        self.expect_punctuation(":");
        let type_name = self.expect_type();
        Parameter { name, type_name }
    }

    fn parse_compound_statement(&mut self) -> Vec<AstNode> {
        self.expect_punctuation("{");
        let mut statements = Vec::new();
        while !self.current_token_is_punctuation("}") {
            statements.push(self.parse_statement());
        }
        self.expect_punctuation("}");
        statements
    }

    fn parse_statement(&mut self) -> AstNode {
        // TODO: Complete this
        match &self.current_token {
            Some(Token::Type(_)) => self.parse_variable_declaration(),
            // TODO: Support other types of statements
            _ => panic!("Unexpected token: {:?}", self.current_token),
        }
    }

    fn parse_variable_declaration(&mut self) -> AstNode {
        let var_type = self.expect_type();
        let identifier = self.expect_identifier();
        let value = if self.current_token_is_operator("=") {
            self.advance();
            Some(Box::new(self.parse_expression()))
        } else {
            None
        };
        self.expect_punctuation(";");

        AstNode::VariableDeclaration {
            var_type,
            identifier,
            value,
        }
    }

    fn parse_expression(&mut self) -> AstNode {
        // TODO: Parse all possible types of expressions and operators
        // just doing a 0 for now
        AstNode::Literal {
            value: LiteralValue::Number("0".to_string()),
        }
    }

    fn parse_if_statement(&mut self) -> AstNode {
        self.expect_keyword("if");
        self.expect_punctuation("(");
        let condition = Box::new(self.parse_expression());
        self.expect_punctuation(")");
        let body = self.parse_compound_statement();
        let else_body = if self.current_token_is_keyword("else") {
            self.advance();
            Some(self.parse_compound_statement())
        } else {
            None
        };

        AstNode::IfStatement {
            condition,
            body,
            else_body,
        }
    }

    fn parse_while_statement(&mut self) -> AstNode {
        self.expect_keyword("while");
        self.expect_punctuation("(");
        let condition = Box::new(self.parse_expression());
        self.expect_punctuation(")");
        let body = self.parse_compound_statement();

        AstNode::WhileStatement { condition, body }
    }

    fn parse_for_statement(&mut self) -> AstNode {
        self.expect_keyword("for");
        self.expect_punctuation("(");
        let initialization = Box::new(self.parse_statement());
        let condition = Box::new(self.parse_expression());
        self.expect_punctuation(";");
        let update = Box::new(self.parse_expression());
        self.expect_punctuation(")");
        let body = self.parse_compound_statement();

        AstNode::ForStatement {
            initialization,
            condition,
            update,
            body,
        }
    }

    fn parse_binary_operation(&mut self) -> AstNode {
        // TODO : write test + check if this is kosher in Dragon
        // basic L + <OP> + R
        let left = Box::new(self.parse_expression());
        let operator = self.parse_operator();
        let right = Box::new(self.parse_expression());

        AstNode::BinaryOperation {
            operator,
            left,
            right,
        }
    }

    fn parse_operator(&mut self) -> BinaryOperator {
        if let Some(Token::Operator(op)) = self.current_token.clone() {
            self.advance();
            match op.as_str() {
                "+" => BinaryOperator::Add,
                "-" => BinaryOperator::Subtract,
                "*" => BinaryOperator::Multiply,
                "/" => BinaryOperator::Divide,
                "==" => BinaryOperator::Equal,
                "!=" => BinaryOperator::NotEqual,
                ">" => BinaryOperator::GreaterThan,
                "<" => BinaryOperator::LessThan,
                ">=" => BinaryOperator::GreaterThanOrEqual,
                "<=" => BinaryOperator::LessThanOrEqual,
                _ => panic!("Unexpected operator: {}", op),
            }
        } else {
            panic!("Expected operator, found {:?}", self.current_token);
        }
    }

    fn parse_literal(&mut self) -> AstNode {
        let token = self.current_token.clone();
        match token {
            Some(Token::Number(num)) => {
                self.advance();
                AstNode::Literal {
                    value: LiteralValue::Number(num),
                }
            }
            Some(Token::StringLiteral(str_lit)) => {
                self.advance();
                AstNode::Literal {
                    value: LiteralValue::StringLiteral(str_lit),
                }
            }
            _ => panic!("Expected literal, found {:?}", self.current_token),
        }
    }

    fn expect_keyword(&mut self, expected: &str) {
        if let Some(Token::Keyword(kw)) = &self.current_token {
            if kw == expected {
                self.advance();
            } else {
                panic!("Expected keyword '{}', found '{}'", expected, kw);
            }
        } else {
            panic!(
                "Expected keyword '{}', found '{:?}'",
                expected, self.current_token
            );
        }
    }

    fn expect_identifier(&mut self) -> String {
        if let Some(Token::Identifier(ident)) = self.current_token.take() {
            self.advance();
            ident
        } else {
            panic!("Expected identifier, found '{:?}'", self.current_token);
        }
    }

    fn expect_type(&mut self) -> String {
        if let Some(Token::Type(ty)) = self.current_token.take() {
            self.advance();
            ty
        } else {
            panic!("Expected type, found '{:?}'", self.current_token);
        }
    }

    fn expect_punctuation(&mut self, expected: &str) {
        if let Some(Token::Punctuation(punct)) = &self.current_token {
            if punct == expected {
                self.advance();
            } else {
                panic!("Expected punctuation '{}', found '{}'", expected, punct);
            }
        } else {
            panic!(
                "Expected punctuation '{}', found '{:?}'",
                expected, self.current_token
            );
        }
    }

    fn current_token_is_punctuation(&self, punct: &str) -> bool {
        matches!(self.current_token.as_ref(), Some(Token::Punctuation(p)) if p == punct)
    }

    fn current_token_is_operator(&self, op: &str) -> bool {
        matches!(self.current_token.as_ref(), Some(Token::Operator(o)) if o == op)
    }

    fn current_token_is_keyword(&self, kw: &str) -> bool {
        matches!(self.current_token.as_ref(), Some(Token::Keyword(k)) if k == kw)
    }

    // TODO: ... Put helpers here etc etc

    fn advance(&mut self) {
        self.tokens.pop_front();
        self.current_token = self.tokens.front().cloned();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn setup_parser(input: &str) -> Parser {
        Parser::new(input)
    }

    #[test]
    fn test_function_parsing() {
        let input = r#"func someFunction(x: int, y: int) -> int { return x + y; }"#;
        let mut parser = setup_parser(input);
        let ast = parser.parse();
    }

    #[test]
    fn test_simple_function() {
        let input = r#"func add(a: int, b: int) -> int { return a + b; }"#;
        let mut parser = setup_parser(input);
        let ast = parser.parse();

        // Check there is one and only one function
        assert_eq!(ast.len(), 1);

        if let AstNode::Function {
            name,
            params,
            return_type,
            body,
        } = &ast[0]
        {
            assert_eq!(name, "add");
            assert_eq!(params.len(), 2);
            assert_eq!(return_type.as_deref(), Some("int"));
        } else {
            panic!("Expected a function definition");
        }
    }

    #[test]
    fn test_complex_multiline_code() {
        let input = r#"
        func calculate(x: int, y: int) -> int {
            var result: int;
            if x > y {
                result = x - y;
            } else {
                result = y - x;
            }
            return result;
        }
    "#;
        let mut parser = setup_parser(input);
        let ast = parser.parse();

        // Check if there's exactly one function
        assert_eq!(ast.len(), 1);

        if let AstNode::Function {
            name,
            params,
            return_type,
            body,
        } = &ast[0]
        {
            // Check function name and return type
            assert_eq!(name, "calculate");
            assert_eq!(return_type.as_deref(), Some("int"));

            // Check parameters
            assert_eq!(params.len(), 2);
            assert_eq!(params[0].name, "x");
            assert_eq!(params[0].type_name, "int");
            assert_eq!(params[1].name, "y");
            assert_eq!(params[1].type_name, "int");

            // 3 because var dec, if statement, return statement
            assert_eq!(body.len(), 3);

            // Check variable declaration
            if let AstNode::VariableDeclaration {
                var_type,
                identifier,
                value,
            } = &body[0]
            {
                assert_eq!(var_type, "int");
                assert_eq!(identifier, "result");
                assert!(value.is_none());
            } else {
                panic!("Expected a variable declaration");
            }

            // Check if statement
            if let AstNode::IfStatement {
                condition,
                body: if_body,
                else_body,
            } = &body[1]
            {
            } else {
                panic!("Expected an if statement");
            }

            // Check return statement
            if let AstNode::ExpressionStatement { expression } = &body[2] {
            } else {
                panic!("Expected a return statement");
            }
        } else {
            panic!("Expected a function definition");
        }
    }
}
