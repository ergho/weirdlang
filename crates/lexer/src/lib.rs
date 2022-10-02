#![allow(dead_code)]

use std::{collections::VecDeque, fmt::Debug, iter::Peekable, ops::Range, str::CharIndices};

#[derive(Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub loc: Range<usize>,
}

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "(Token: {:?}, Location: {:?}) ", self.kind, self.loc,)
    }
}

impl Token {
    fn new_at(kind: TokenKind, loc: usize) -> Self {
        Self::new(kind, loc..loc + 1)
    }

    fn new(kind: TokenKind, loc: Range<usize>) -> Self {
        Self { kind, loc }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Illegal,
    EndOfFile,

    // identifiers + literals
    Identifier(String),
    Integer(String),

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

pub struct Lexer<'a> {
    input: &'a str,
    chars: Peekable<CharIndices<'a>>,
}

impl<'c> Debug for Lexer<'c> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Lexer ch {:?} ", self.chars)
    }
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let chars = input.char_indices().peekable();
        Self {
            input,
            chars,
        }
    }

    pub fn next_token(&mut self) -> Token {
        loop {
            break match self.chars.next() {
                Some((_, ch)) if ch.is_ascii_whitespace() => {
                    continue;
                }
                Some((offset, ch)) => match ch {
                    '=' => self.handle_equal(offset),
                    '+' =>  Token::new_at(TokenKind::Plus, offset),
                    '!' => self.handle_exclamation_mark(offset),
                    '-' => Token::new_at(TokenKind::Minus, offset),
                    '(' => Token::new_at(TokenKind::LeftParen, offset),
                    ')' => Token::new_at(TokenKind::RightParen, offset),
                    '{' => Token::new_at(TokenKind::LeftBrace, offset),
                    '}' => Token::new_at(TokenKind::RightBrace, offset),
                    '[' => Token::new_at(TokenKind::LeftBracket, offset),
                    ']' => Token::new_at(TokenKind::RightBracket, offset),
                    ';' => Token::new_at(TokenKind::Semicolon, offset),
                    ',' => Token::new_at(TokenKind::Comma, offset),
                    '/' => Token::new_at(TokenKind::Division, offset),
                    '>' => Token::new_at(TokenKind::GreaterThan, offset),
                    '<' => Token::new_at(TokenKind::LessThan, offset),
                    '*' => Token::new_at(TokenKind::Multiplication, offset),
                    _ => {
                        if ch.is_ascii_alphabetic() || ch == '_' {
                            return self.read_identifier(offset)
                        } else if ch.is_ascii_digit() {
                            self.read_number(offset)
                        } else {
                            Token::new_at(TokenKind::Illegal, offset)
                        }
                    }
                },
                _ => Token::new_at(TokenKind::EndOfFile, 0)
            };
        }
    }

    fn read_identifier(&mut self, offset: usize)  -> Token {
        let mut end = offset;

        while self.chars.peek().map_or(false, |(_, ch)| ch.is_ascii_alphabetic()) {
            end = self.chars.next().expect("safe, just peeked").0;
        }
        let kind = match &self.input[offset..=end] {
            "let" => TokenKind::Let,
            "fn" =>  TokenKind::Function,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "return" => TokenKind::Return,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            _ => TokenKind::Identifier(self.input[offset..=end].to_string())

        };
        Token {

            kind,
            loc: offset..end + 1
        }

    }

    fn read_number(&mut self, offset: usize) -> Token {

        let mut end = offset;

        while self.chars.peek().map_or(false, |(_, ch)| ch.is_ascii_digit()) {
            end = self.chars.next().expect("safe, just peeked").0;
        }

        let value = self.input[offset..=end].to_string();

        Token {
            kind:TokenKind::Integer(value),
            loc: offset..end + 1
        }
    }

    fn read_one(&mut self, kind: TokenKind, offset: usize) -> Token {
        Token::new(kind, offset..offset + 1)
    }
    fn read_two(&mut self, kind: TokenKind, offset: usize) -> Token {
        self.chars.next();
        Token::new(kind, offset..offset + 2)
    }

    fn handle_equal(&mut self, offset: usize) -> Token {
        let peeked = self.chars.peek().unwrap();

        match peeked {
            (_, '=') => self.read_two(TokenKind::Equal, offset),
            _ => self.read_one(TokenKind::Assign, offset),
        }
    }
    fn handle_exclamation_mark(&mut self, offset: usize) -> Token {
        let peeked = self.chars.peek().unwrap();

        match peeked {
            (_,'=') => self.read_two(TokenKind::NotEqual, offset),
            _ => self.read_one(TokenKind::ExclamationMark, offset),
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
    Lexer::new(input)
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
            panic!("Failure, {:?}", lexer.input[tok.loc.start..tok.loc.end].to_string());
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

}
