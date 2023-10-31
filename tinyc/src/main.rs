mod lexer;

fn main() {
    let input = r"int main() {
        // This is a comment
        int arr[10];
        arr[5] = 100;
        int* ptr;
        int x = 10;
        ptr = &x;
        return arr[5] + *ptr;
    }";

    let tokens = lexer::lex_c_code(input);
    for token in tokens {
        println!("{:?}", token);
    }
}
