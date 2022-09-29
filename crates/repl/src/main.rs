use std::io::Write;

const PROMPT: &str = ">> ";

fn start(input: std::io::Stdin, mut output: std::io::Stdout) {
    loop {
        let mut buffer = String::new();
        print!("{}", PROMPT);
        output.flush().expect("flushing failed");
        input.read_line(&mut buffer).expect("cant read line");
        let buffer = buffer.trim();
        let mut lex = lexer::Lexer::new(buffer);
        lex.read_char();
        loop {
            let tok = lex.next_token();
            if tok.kind == lexer::TokenKind::EndOfFile {
                break;
            }
            if tok.kind == lexer::TokenKind::Illegal {
                panic!("Failed with {:?}", tok.val);
            }
            println!("{:?}", tok);
        }
    }
}
fn main() {
    println!("Hello, world!");

    start(std::io::stdin(), std::io::stdout());
}
