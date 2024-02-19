mod lexer;
mod parser;
mod codegen;

fn main() {
    let input = r#"
        func main() -> int {
            // This is a comment
            int x = 10;
            int y = 20;
            return x + y;
        }
    "#;

    let tokens = lexer::lex_yavanna_code(input);
    for token in tokens {
        println!("{:?}", token);
    }
}
