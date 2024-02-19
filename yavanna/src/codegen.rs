use crate::parser::{AstNode, BinaryOperator, LiteralValue, Parameter};

pub struct CodeGenerator;

impl CodeGenerator {
    pub fn generate(ast: &[AstNode]) -> String {
        let mut assembly_code = String::new();
        for node in ast {
            assembly_code += &Self::generate_node(node); 
        }
        assembly_code
    }

    fn generate_node(node: &AstNode) -> String {
        match node {
            AstNode::Function { name, params, body, .. } => {
                let mut func_asm = format!(".global {}\n{}:\n", name, name);
                for (index, param) in params.iter().enumerate() {
                    func_asm += &format!("\t@ Parameter: {} {} in r{}\n", param.type_name, param.name, index);
                }
                for statement in body {
                    func_asm += &Self::generate_node(statement); 
                }
                func_asm += "\tbx lr\n";
                func_asm
            },
            AstNode::ReturnStatement { value } => {
                let expr_asm = Self::generate_node(value);
                format!("{}\tmov lr, pc\n\tpop {{pc}}\n", expr_asm)
            },
            AstNode::Literal { value } => match value {
                LiteralValue::Number(num) => format!("\tmov r0, #{}\n", num),
                // TODO: Handle all literal types
                _ => String::new(),
            },
            AstNode::BinaryOperation { operator, left, right } => {
                let lhs_asm = Self::generate_node(left);
                let rhs_asm = Self::generate_node(right);
                let op_asm = match operator {
                    BinaryOperator::Add => "\tadd r0, r0, r1\n",
                    BinaryOperator::Subtract => "\tsub r0, r0, r1\n",
                    // TODO: Handle all operators
                    _ => "",
                };
                format!("{}{}{}", lhs_asm, rhs_asm, op_asm)
            },
            AstNode::VariableDeclaration { var_type, identifier, value } => {
                let value_asm = value.as_ref().map_or(String::new(), |expr| Self::generate_node(expr));
                format!("{}@ Declaring variable: {} of type {}\n", value_asm, identifier, var_type)
            },
            // TODO: Handle all node types
            _ => String::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{Parser};
    use crate::lexer::{lex_yavanna_code};

    #[test]
    fn test_code_generation() {
        let input = r#"
        func add(x: int, y: int) -> int {
            return x;
        }
        "#;
        let tokens = lex_yavanna_code(input);
        let ast = Parser::new(tokens).parse();
        let asm = CodeGenerator::generate(&ast);
        assert!(!asm.is_empty());
        println!("Generated ASM:\n{}", asm);
    }
}
