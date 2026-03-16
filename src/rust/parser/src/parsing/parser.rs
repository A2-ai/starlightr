use fs_err as fs;
use std::fmt;
use std::path::Path;

use anyhow::Result as AnyhowResult;

use crate::Document; 
use crate::document::Node;
use crate::parsing::lexer::{Token, lex};
use crate::parsing::span::{Span, Spanned};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParserError {
    message: String,
    span: Span,
}

impl ParserError {
    pub fn new(message: String, span: Span) -> Self {
        Self {
            message,
            span,
        }
    }
}

impl std::error::Error for ParserError {}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

pub struct Parser {
    tokens: Vec<Spanned<Token>>,
    index: usize,
}

impl Parser {
    pub fn new(content: &str) -> Self {
        let tokens = lex(content);
        Self { tokens, index: 0 }
    }

    fn is_eof(&self) -> bool {
        self.tokens[self.index].value == Token::Eof
    }

    fn peek(&self) -> &Spanned<Token> {
        &self.tokens[self.index]
    }

    fn advance(&mut self) -> Spanned<Token> {
        if !self.is_eof() {
            self.index += 1;
        }
        self.tokens[self.index - 1].clone()
    }

    fn is_trivia(&self) -> bool {
        matches!(
            self.peek().value, Token::Comment(_)
        )
    }

    fn skip_trivia(&mut self) {
        while self.is_trivia() {
            self.advance();
        }
    }

    fn parse_group(&mut self, open: &Token, close: &Token) -> Result<Vec<Node>, ParserError> {
        if &self.peek().value == open {
            self.advance();
            self.skip_trivia()
        } else {
            return Err(ParserError::new(
                format!("Expected '{}' Token, got {}", open, self.peek().value),
                self.peek().span,
            ));
        };

        let mut children = Vec::new();

        loop {
            self.skip_trivia();

            if &self.peek().value == close {
                self.advance();
                return Ok(children);
            } else if &self.peek().value == open {
                let sub_group = self.parse_group(open, close)?;
                children.extend(sub_group);
            } else if self.is_eof() {
                return Err(ParserError::new(
                    "Reached EOF unexpectedly".to_string(),
                    self.peek().span,
                ));
            } else {
                let n = self.parse_node()?;
                children.push(n);
            }
        }
    }

    fn parse_bracket_group(&mut self) -> Result<Vec<Node>, ParserError> {
        self.parse_group(&Token::LeftBracket, &Token::RightBracket)
    }

    fn parse_brace_group(&mut self) -> Result<Vec<Node>, ParserError> {
        self.parse_group(&Token::LeftBrace, &Token::RightBrace)
    }

    fn parse_command_node(&mut self, command: String) -> Result<Node, ParserError> {
        let mut args = Vec::new();
        let mut option = Vec::new();

        // \command[option]{arg}{args}
        while self.peek().value == Token::LeftBracket {
            option.push(self.parse_bracket_group()?);
        }

        while self.peek().value == Token::LeftBrace {
            args.push(self.parse_brace_group()?);
        }

        let options = if option.is_empty() {
            None
        } else {
            Some(option)
        };

        Ok(Node::Command {
            name: command,
            options,
            args,
        })
    }

    fn parse_node(&mut self) -> Result<Node, ParserError> {
        self.skip_trivia();
        let tok = self.advance();

        match tok.value {
            Token::Text(s) => Ok(Node::Text(s)),
            Token::EscapedChar(c) => Ok(Node::EscapedChar(c)),
            Token::Command(s) => self.parse_command_node(s),
            Token::Newline => Ok(Node::NewLine),
            Token::LeftBracket => Ok(Node::Text("[".to_string())),
            Token::RightBracket => Ok(Node::Text("]".to_string())),
            _ => Err(ParserError::new(
                format!("Encountered unexpected trivia token: {}", tok.value),
                tok.span,
            )),
        }
    }

    fn parse_document(&mut self) -> Result<Document, ParserError> {
        let mut children = Vec::new();

        while !self.is_eof() {
            self.skip_trivia();
            if self.is_eof() {
                break;
            } else {
                let node = self.parse_node()?;
                children.push(node)
            }
        }

        Ok(Document { children })
    }
}

pub fn parse_file(path: impl AsRef<Path>) -> AnyhowResult<Document> {
    let contents = fs::read_to_string(path)?;
    parse(&contents)
}

pub fn parse(contents: &str) -> AnyhowResult<Document> {
    let mut parser = Parser::new(contents);
    let doc = parser.parse_document()?;
    let doc = doc.lower();    

    Ok(doc)
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::{assert_debug_snapshot, glob};
    use std::path::PathBuf;

    #[test]
    fn can_parse_rd_files() {
        let test_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test_data");
        glob!(test_dir, "*.Rd", |path| {
            assert_debug_snapshot!(parse_file(path).unwrap());
        });
    }
}
