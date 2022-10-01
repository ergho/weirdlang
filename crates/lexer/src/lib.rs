#![feature(let_chains)]
#![allow(dead_code)]

use std::{collections::VecDeque, fmt::Debug};

#[derive(Clone, PartialEq)]
struct Span {
    start_row: usize,
    start_col: usize,
    end_row: usize,
    end_col: usize,
}

impl Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "(Start: {},{}, End: {},{})",
            self.start_row, self.start_col, self.end_row, self.end_col,
        )
    }
}

#[derive(Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub val: String,
    //span: Span,
}

// add span to debug implementation when ready for it
impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({:?},{:?})", self.kind, self.val,)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Illegal,
    EndOfFile,

    // identifiers + literals
    Identifier,
    Integer,

    // Operators
    Assign,
    Plus,
    Minus,
    Equal,
    Multiplication,
    Modulus,
    Division,
    Percent,
    PlusEqual,
    MinusEqual,
    MultiEqual,
    DivEqual,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    ExclamationMark,

    // Reserved
    True,
    False,

    // Delimiters
    Comma,
    Semicolon,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,

    // Keywords
    Function,
    Let,
    Return,
    If,
    Else,
}

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: Option<char>,

    chars: Vec<char>,
}

impl Debug for Lexer {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Lexer ch {:?} ", self.ch)
    }
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let input = input.to_string();
        let chars = input.chars().collect();
        Self {
            input,
            position: 0,
            read_position: 0,
            ch: None,
            chars,
        }
    }

    pub fn read_char(&mut self) {
        match self.chars.get(self.read_position) {
            Some(&c) => {
                self.ch = Some(c);
                self.position = self.read_position;
                self.read_position += 1;
            }
            None => {
                self.ch = None;
            }
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let tok = match self.ch {
            Some(ch) => match ch {
                '=' => self.handle_equal(),
                '!' => self.handle_exclamation_mark(),

                '+' => Token {
                    kind: TokenKind::Plus,
                    val: "+".to_string(),
                },
                ';' => Token {
                    kind: TokenKind::Semicolon,
                    val: ";".to_string(),
                },
                '-' => Token {
                    kind: TokenKind::Minus,
                    val: "-".to_string(),
                },
                '(' => Token {
                    kind: TokenKind::LeftParen,
                    val: "(".to_string(),
                },
                ')' => Token {
                    kind: TokenKind::RightParen,
                    val: ")".to_string(),
                },
                '{' => Token {
                    kind: TokenKind::LeftBrace,
                    val: "{".to_string(),
                },
                '}' => Token {
                    kind: TokenKind::RightBrace,
                    val: "}".to_string(),
                },
                ',' => Token {
                    kind: TokenKind::Comma,
                    val: ",".to_string(),
                },
                '*' => Token {
                    kind: TokenKind::Multiplication,
                    val: "*".to_string(),
                },
                '/' => Token {
                    kind: TokenKind::Division,
                    val: "/".to_string(),
                },
                '<' => Token {
                    kind: TokenKind::LessThan,
                    val: "<".to_string(),
                },
                '>' => Token {
                    kind: TokenKind::GreaterThan,
                    val: ">".to_string(),
                },
                _ => {
                    if is_letter(ch) || ch == '_' {
                        return self.read_identifier();
                    } else if ch.is_ascii_digit() {
                        return self.read_number();
                    } else {
                        Token {
                            kind: TokenKind::Illegal,
                            val: ch.to_string(),
                        }
                    }
                }
            },
            None => Token {
                kind: TokenKind::EndOfFile,
                val: "".to_string(),
            },
        };
        self.read_char();
        tok
    }

    fn read_identifier(&mut self) -> Token {
        let position = self.position;
        while let Some(ch) = self.ch && is_identifier(ch) {
            self.read_char();
        }

        let val: String = self.chars[position..self.position].iter().collect();

        let kind = match val.as_str() {
            "let" => TokenKind::Let,
            "fn" => TokenKind::Function,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "return" => TokenKind::Return,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,

            _ => TokenKind::Identifier,
        };

        Token { kind, val }
    }

    fn read_number(&mut self) -> Token {
        let position = self.position;
        while let Some(ch) = self.ch && ch.is_numeric() {
            self.read_char();
        }

        Token {
            kind: TokenKind::Integer,
            val: self.chars[position..self.position].iter().collect(),
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.ch && ch.is_ascii_whitespace() {
            self.read_char();
        }
    }

    fn peek_char(&self) -> Option<&char> {
        self.chars.get(self.position + 1)
    }

    fn read_one(&mut self, kind: TokenKind) -> Token {
        Token {
            kind,
            val: self.ch.unwrap().to_string(),
        }
    }
    fn read_two(&mut self, kind: TokenKind) -> Token {
        let position = self.position;
        self.read_char();
        Token {
            kind,
            val: self.chars[position..=self.position].iter().collect(),
        }
    }

    fn handle_equal(&mut self) -> Token {
        let peeked = self.peek_char().unwrap();

        match peeked {
            '=' => self.read_two(TokenKind::Equal),
            _ => self.read_one(TokenKind::Assign),
        }
    }
    fn handle_exclamation_mark(&mut self) -> Token {
        let peeked = self.peek_char().unwrap();

        match peeked {
            '=' => self.read_two(TokenKind::NotEqual),
            _ => self.read_one(TokenKind::ExclamationMark),
        }
    }
}

fn is_letter(ch: char) -> bool {
    ('a'..='z').contains(&ch) || ('A'..='Z').contains(&ch)
}

fn is_identifier(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_'
}

pub fn new_lexer(input: &str) -> Lexer {
    let mut l = Lexer::new(input);
    l.read_char();
    l
}

pub fn test_lexing(input: &str) -> String {
    let mut lexer = new_lexer(input);

    let mut tokens = VecDeque::new();

    loop {
        let tok = lexer.next_token();
        if tok.kind == TokenKind::EndOfFile {
            break;
        }

        if tok.kind == TokenKind::Illegal {
            panic!("Failure, {:?}", tok.val);
        }
        tokens.push_back(tok);
    }

    let mut output = String::new();

    while let Some(tok) = tokens.pop_front() {
        output += &format!(" {:?}", tok);
        output += "\n"
    }

    output
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! test_next_token {
        ($name:ident, $path:expr) => {
            #[test]
            fn $name() {
                let contents = include_str!($path);
                let mut settings = insta::Settings::clone_current();
                settings.set_snapshot_path("../testdata/output");
                settings.bind(|| {
                    insta::assert_snapshot!(test_lexing(contents));
                });
            }
        };
    }
    test_next_token!(test_example1, "../testdata/input/example1.wl");
    test_next_token!(test_example2, "../testdata/input/example2.wl");
    test_next_token!(test_example3, "../testdata/input/example3.wl");
    test_next_token!(test_example4, "../testdata/input/example4.wl");

    //#[test]
    //fn test_files() {
    //    insta::glob!("testdata/input/*.wl", |path| {
    //        let contents = std::fs::read_to_string(&path).unwrap();
    //        let mut settings = insta::Settings::clone_current();
    //        settings.set_snapshot_path("../testdata/output");
    //        settings.bind(|| {
    //            insta::assert_snapshot!(test_lexing(&contents));
    //        });
    //    });
    //}
}
