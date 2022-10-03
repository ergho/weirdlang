#![deny(clippy::all)]
#![warn(clippy::pedantic)]
#![allow(dead_code)]
use lexer::new_lexer;
use lexer::Lexer;
use lexer::Token;
use lexer::TokenKind;

#[derive(Debug)]
struct Program {
    statements: Vec<Statement>,
}

#[derive(Debug, PartialEq)]
enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

#[derive(Debug, PartialEq)]
struct LetStatement {
    token: Token,
    name: String,
    assign: Token,
    expr: Expression,
}

impl LetStatement {
    fn parse(parser: &mut Parser) -> Result<Statement, ParseError> {
        let lstmt = LetStatement {
            token: parser.expect_token(TokenKind::Let)?,
            name: parser.parse_identifier()?,
            assign: parser.expect_token(TokenKind::Assign)?,
            expr: Expression::parse(parser)?,
        };
        Ok(Statement::Let(lstmt))
    }
}

#[derive(Debug, PartialEq)]
struct ReturnStatement {
    token: Token,
    expr: Expression,
}

impl ReturnStatement {
    fn parse(parser: &mut Parser) -> Result<Statement, ParseError> {
        let retstmt = ReturnStatement {
            token: parser.expect_token(TokenKind::Return)?,
            expr: Expression::parse(parser)?,
        };
        Ok(Statement::Return(retstmt))
    }
}

#[derive(Debug, PartialEq)]
struct ExpressionStatement {
    token: Token,
    expr: Expression,
}

impl ExpressionStatement {
    fn parse() -> Result<Statement, ParseError> {
        todo!();
    }
}

#[derive(Debug, PartialEq)]
enum Expression {
    Ident(Identifier),
    Number(Number),
}

#[derive(Debug)]
enum Prefix {
    ExclamationMark,
    Minus,
}

impl Expression {
    fn parse(parser: &mut Parser) -> Result<Expression, ParseError> {
        match &parser.current_token.kind {
            TokenKind::Integer(n) => {
                Ok(Expression::Number(Number { val: n.to_string() }))
            }
            TokenKind::Identifier(s) => {
                Ok(Expression::Ident(Identifier { val: s.to_string() }))
            }
            _ => Err(ParseError::OutOfPlace {
                got: parser.current_token.kind.clone(),
                location: parser.current_token.loc.clone(),
            }),
        }
    }
}

#[derive(Debug, PartialEq)]
struct Number {
    val: String,
}

#[derive(Debug, PartialEq)]
struct Identifier {
    val: String,
}

#[derive(Debug)]
struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
    peek_token: Token,
    errors: Vec<ParseError>,
}

impl<'a> Parser<'a> {
    fn new(mut lexer: Lexer<'a>) -> Self {
        Self {
            current_token: lexer.next_token(),
            peek_token: lexer.next_token(),
            errors: vec![],
            lexer,
        }
    }

    fn errors(&self) -> &[ParseError] {
        &self.errors
    }

    fn skip_until_token(&mut self, kind: &TokenKind) {
        while &self.current_token.kind != kind {
            self.next_token();
        }
    }
    fn expect_token(&mut self, kind: TokenKind) -> Result<Token, ParseError> {
        let token = self.current_token.clone();
        if token.kind != kind {
            self.skip_until_token(&TokenKind::Semicolon);
            return Err(ParseError::Unexpected {
                got: token.kind,
                expected: kind,
            });
        }
        self.next_token();
        Ok(token)
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
        //    self.current_token.clone()
    }

    fn parse_prefix(&mut self) -> Result<Expression, ParseError> {
        let _prefix = match self.current_token.kind {
            TokenKind::ExclamationMark => Prefix::ExclamationMark,
            TokenKind::Minus => Prefix::Minus,
            _ => todo!(),
        };
        self.next_token();

        todo!();

    }
    fn parse_program(&mut self) -> Program {
        let mut program = Program { statements: vec![] };
        while self.current_token.kind != TokenKind::EndOfFile {
            match self.parse_statement() {
                Ok(stmt) => program.statements.push(stmt),
                Err(err) => self.errors.push(err),
            }
        }
        program
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        let stmt: Result<Statement, ParseError> = match self.current_token.kind
        {
            TokenKind::Let => self.parse_statement_let(),
            TokenKind::Return => self.parse_statement_return(),
            _ => panic!(),
        };
        self.next_token();
        stmt
    }

    fn parse_statement_let(&mut self) -> Result<Statement, ParseError> {
        let lstmt = LetStatement::parse(self)?;
        while self.current_token.kind != TokenKind::Semicolon {
            self.next_token();
        }
        Ok(lstmt)
    }

    fn parse_statement_return(&mut self) -> Result<Statement, ParseError> {
        let retstmt = ReturnStatement::parse(self)?;
        while self.current_token.kind != TokenKind::Semicolon {
            self.next_token();
        }
        Ok(retstmt)
    }

    fn parse_identifier(&mut self) -> Result<String, ParseError> {
        let ident = match &self.current_token.kind {
            TokenKind::Identifier(ident) => Ok(ident.to_string()),
            _ => Err(ParseError::Unexpected {
                got: self.current_token.kind.clone(),
                expected: TokenKind::Identifier("a".to_string()),
            }),
        };

        if ident.is_err() {
            println!("We are skipping {:?}", self.current_token.kind.clone());
            println!("Peeking at {:?}", self.peek_token.kind.clone());
            self.skip_until_token(&TokenKind::Semicolon);
            return ident;
        }
        self.next_token();
        ident
    }
}
fn snapshot_parsing(input: &str) -> String {
    let lexer = new_lexer(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    if parser.errors.is_empty() {
        format!("{:#?}", program.statements)
    } else {
        format!("{:#?}", parser.errors)
    }
}

#[derive(Debug)]
enum ParseError {
    Unexpected {
        got: lexer::TokenKind,
        expected: lexer::TokenKind,
    },
    OutOfPlace {
        got: lexer::TokenKind,
        location: std::ops::Range<usize>,
    },
}

impl std::error::Error for ParseError {}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unexpected { got, expected } => {
                write!(
                    f,
                    "Got token: {:?}, Expected  token: {:?}",
                    got, expected
                )
            }
            Self::OutOfPlace { got, location } => {
                write!(
                    f,
                    "Could not parse token {:?}, Location: {:?}",
                    got, location,
                )
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! snapshot {
        ($name:tt, $path:tt) => {
            #[test]
            fn $name() {
                let contents = include_str!($path);
                let mut settings = insta::Settings::clone_current();
                settings.set_snapshot_path("../testdata/output/");
                settings.bind(|| {
                    insta::assert_snapshot!(snapshot_parsing(contents));
                });
            }
        };
    }

    snapshot!(test_var, "../testdata/input/let_test.wl");
    snapshot!(test_return, "../testdata/input/return_test.wl");
    snapshot!(test_busted_let, "../testdata/input/let_busted_test.wl");
}
