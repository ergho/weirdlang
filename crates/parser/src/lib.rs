#![allow(dead_code)]
use anyhow::Result;
use lexer::{new_lexer, Lexer, Token, TokenKind};

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
    fn parse(parser: &mut Parser) -> Result<Statement> {
        let lstmt = LetStatement {
            token: parser.expect_token(TokenKind::Let)?,
            name: parser.expect_token(TokenKind::Identifier)?.val,
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
    fn parse(parser: &mut Parser) -> Result<Statement> {
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
    fn parse(parser: &mut Parser) -> Result<Statement> {
        todo!();
    }
}

#[derive(Debug, PartialEq)]
enum Expression {
    Ident(Identifier),
    Number(Number),
}

impl Expression {
    fn parse(parser: &mut Parser) -> Result<Expression> {
        Ok(match parser.current_token.kind {
            TokenKind::Integer => Expression::Number(Number {
                val: parser.current_token.val.clone(),
            }),
            _ => todo!("not yet done"),
        })

        //while parser.current_token.kind != TokenKind::Semicolon {
        //    println!("curr{:#?}", parser.current_token.kind);
        //    println!("peek{:#?}", parser.peek_token.kind);
        //    thing +=1;
        //    println!("{}", thing);
        //    parser.next_token();
        //}
        //parser.next_token();
        //Ok(num)
    }
}

#[derive(Debug, PartialEq)]
struct Number {
    val: String,
}

#[derive(Debug, PartialEq)]
struct Identifier {
    token: Token,
}

#[derive(Debug)]
struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl Parser {
    fn new(mut lexer: Lexer) -> Self {
        Self {
            current_token: lexer.next_token(),
            peek_token: lexer.next_token(),
            errors: vec![],
            lexer,
        }
    }

    fn errors(&self) -> &[String] {
        &self.errors
    }

    fn skip_until_toke(&mut self, kind: TokenKind) {
        while self.current_token.kind != kind {
            self.next_token();
        }
    }
    fn expect_token(&mut self, kind: TokenKind) -> Result<Token> {
        let token = self.current_token.clone();
        if token.kind != kind {
            return Err(anyhow::anyhow!(
                "Got token: {:?}, Expected token: {:?}",
                token,
                kind
            ));
        }
        self.next_token();
        Ok(token)
    }

    fn next_token(&mut self) -> Token {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
        self.current_token.clone()
    }

    fn parse_program(&mut self) -> Program {
        let mut program = Program { statements: vec![] };
        while self.current_token.kind != TokenKind::EndOfFile {
            //println!("parsing statement {:#?}", self);
            // println!("Statements: {:#?}", program.statements);
            let statement = self.parse_statement();
            if let Ok(stmt) = statement {
                program.statements.push(stmt);
            }
        }
        program
    }

    fn parse_statement(&mut self) -> Result<Statement> {
        let stmt = match self.current_token.kind {
            TokenKind::Let => self.parse_statement_let()?,
            TokenKind::Return => self.parse_statement_return()?,
            _ => panic!("{:#?}", self.current_token.kind),
        };
        self.next_token();
        Ok(stmt)
    }

    fn parse_statement_let(&mut self) -> Result<Statement> {
        let lstmt = LetStatement::parse(self)?;
        while self.current_token.kind != TokenKind::Semicolon {
            self.next_token();
        }
        Ok(lstmt)
    }

    fn parse_statement_return(&mut self) -> Result<Statement> {
        let retstmt = ReturnStatement::parse(self).unwrap();
        while self.current_token.kind != TokenKind::Semicolon {
            self.next_token();
        }
        Ok(retstmt)
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
