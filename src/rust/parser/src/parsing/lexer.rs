use serde::{Deserialize, Serialize};
use std::fmt;
use std::fmt::Formatter;

use crate::parsing::span::{Span, Spanned};


#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub enum Token {
    Text(String),
    Command(String),
    EscapedChar(char),
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comment(String),
    Newline,
    Eof
}


impl fmt::Debug for Token {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Token::Text(s) => write!(f, "TEXT({s})"),
            Token::Command(s) => write!(f, "COMMAND({s})"),
            Token::EscapedChar(c) => write!(f, "ESCAPED({c})"),
            Token::LeftBrace => write!(f, "LEFT_BRACE"),
            Token::RightBrace => write!(f, "RIGHT_BRACE"),
            Token::LeftBracket => write!(f, "LEFT_BRACKET"),
            Token::RightBracket => write!(f, "RIGHT_BRACKET"),
            Token::Comment(s) => write!(f, "COMMENT({s})"),
            Token::Newline => write!(f, "NEWLINE"),
            Token::Eof => write!(f, "EOF"),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Token::Text(s) => write!(f, "{s}"),
            Token::Command(s) => write!(f, "\\{s}"),
            Token::EscapedChar(c) => write!(f, "{c}"),
            Token::LeftBrace => write!(f, "{{"),
            Token::RightBrace => write!(f, "}}"),
            Token::LeftBracket => write!(f, "["),
            Token::RightBracket => write!(f, "]"),
            Token::Comment(s) => write!(f, "%{s}"),
            Token::Newline => writeln!(f),
            Token::Eof => write!(f, "EOF"),
        }
    }
}

fn is_text_boundary(c: char) -> bool {
    matches!(c, '\\' | '{' | '}' | '[' | ']' | '\n' | '%')
}

pub struct Lexer<'a> {
    input: &'a str,
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(contents: &'a str) -> Self {
        Self {
            input: contents,
            pos: 0,
        }
    }
    
    fn reset(&mut self) {
        self.pos = 0;
    }

    fn mark(&self) -> usize {
        self.pos
    }

    fn peek(&self) -> Option<char> {
        self.input[self.pos..].chars().next() 
    }

    fn advance(&mut self) -> Option<char> {
        let ch = self.peek()?;
        self.pos += ch.len_utf8();
        Some(ch)

    }

    fn take_while<F>(&mut self, mut pred: F) -> &'a str
    where 
        F: FnMut(char) -> bool,
    {
        let start = self.pos;
        while let Some(c) = self.peek() {
            if !pred(c) { break; }
            self.advance();
        }
        &self.input[start..self.pos]
    }
    
    fn lex_command_or_escape(&mut self) -> Spanned<Token> {
        let start = self.mark();        
        //should be at '\' so advance
        self.advance();
        
        // If first char is alphabetic take all alphanumeric 
        // otherwise it is a command escaped char
        let tok = if let Some(c) = self.peek() && c.is_ascii_alphabetic() {
            let func = self.take_while(|c| c.is_ascii_alphanumeric());
            Token::Command(func.to_string())
        } else if let Some(ch) = self.advance() {
            Token::EscapedChar(ch)
        } else {
            Token::Text("\\".to_string())
        };

        Spanned::new(
             tok,
             Span::new(start, self.mark())
        )

    }
   
    fn lex_comment(&mut self) -> Spanned<Token> {
        let start = self.mark();
        // Should be at '%' so advance
        self.advance();

        let comment = self.take_while(|c| c != '\n');
        Spanned::new(
            Token::Comment(comment.to_string()),
            Span::new(start, self.pos)
        )
    }

    fn lex_text(&mut self) -> Spanned<Token> {
        let start = self.mark();
        //Not at a token boundary
        let text = self.take_while(|c| !is_text_boundary(c));
        
        Spanned::new(
            Token::Text(text.to_string()),
            Span::new(start, self.pos)
        )
    }

    fn next_token(&mut self) -> Spanned<Token> {
        let start = self.mark();
        
        let tok = match self.peek() {
            None => Token::Eof,
            Some('{') => { self.advance(); Token::LeftBrace }
            Some('}') => { self.advance(); Token::RightBrace }
            Some('[') => { self.advance(); Token::LeftBracket }
            Some(']') => { self.advance(); Token::RightBracket }
            Some('\n') => { self.advance(); Token::Newline }

            Some('%') => return self.lex_comment(),
            Some('\\') => return self.lex_command_or_escape(),
            Some(_) => return self.lex_text(),
        };

        Spanned::new(tok, Span::new(start, self.mark()))
    }

    fn lex_all(&mut self) -> Vec<Spanned<Token>> {
        self.reset(); 

        let mut lexed = Vec::new();
        lexed.push(self.next_token());

        while let Some(t) = lexed.last() && t.value != Token::Eof {
            lexed.push(self.next_token())
        }

        lexed 
    }
}

pub fn lex(contents: &str) -> Vec<Spanned<Token>> {
    let mut lx = Lexer::new(contents);
    lx.lex_all()
}

#[cfg(test)]
mod tests {
    use super::*;
    use fs_err as fs;
    use std::path::PathBuf;
    use insta::{assert_debug_snapshot, glob};
    
    #[test]
    fn hello_world_lexes() {
        let contents = "hello world";
        let tokens = lex(contents);
        assert!(tokens[0].value == Token::Text("hello world".to_string()));
        assert!(tokens[1].value == Token::Eof);
    }

    #[test]
    fn command_with_digits_works() {
        let contents = r"\S3method{print,foo}";
        let tokens = lex(contents);
        assert!(tokens[0].value == Token::Command("S3method".to_string()));
        assert!(tokens[1].value == Token::LeftBrace);
        assert!(tokens[2].value == Token::Text("print,foo".to_string()));
        assert!(tokens[3].value == Token::RightBrace);
        assert!(tokens[4].value == Token::Eof);
    }
    
    #[test]
    fn can_lex_rd_files() {

        let test_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test_data");
        glob!(test_dir, "*.Rd", |path| {

            let contents = fs::read_to_string(path).unwrap();
            assert_debug_snapshot!(lex(&contents));
        });
    }
}
