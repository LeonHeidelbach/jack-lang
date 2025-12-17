////////////////////////////////////////////////////////////////////////////////
// File: src/internal/tokenize.rs
// Description: Token definitions
//
// Author: Leon Heidelbach <leon.heidelbach@hhu.de>
// Date: 15.05.2024
//
// License: GPLv3
////////////////////////////////////////////////////////////////////////////////

use std::{collections::VecDeque, fmt::Display, iter::Peekable, str::Chars};

use super::ast::Operator;

use shared::{
  error_fmt_file, error_panic, error_panic_fmt_file, io::os::FileInfo,
};

////////////////////////////////////////////////////////////////////////////////
// Constants
////////////////////////////////////////////////////////////////////////////////

// Keywords
pub(crate) const KEYWORD_BOOLEAN: &str = "boolean";
pub(crate) const KEYWORD_CHAR: &str = "char";
pub(crate) const KEYWORD_CLASS: &str = "class";
pub(crate) const KEYWORD_CONSTRUCTOR: &str = "constructor";
pub(crate) const KEYWORD_DO: &str = "do";
pub(crate) const KEYWORD_ELSE: &str = "else";
pub(crate) const KEYWORD_FALSE: &str = "false";
pub(crate) const KEYWORD_FIELD: &str = "field";
pub(crate) const KEYWORD_FUNCTION: &str = "function";
pub(crate) const KEYWORD_IF: &str = "if";
pub(crate) const KEYWORD_INT: &str = "int";
pub(crate) const KEYWORD_LET: &str = "let";
pub(crate) const KEYWORD_METHOD: &str = "method";
pub(crate) const KEYWORD_NULL: &str = "null";
pub(crate) const KEYWORD_RETURN: &str = "return";
pub(crate) const KEYWORD_STATIC: &str = "static";
pub(crate) const KEYWORD_THIS: &str = "this";
pub(crate) const KEYWORD_TRUE: &str = "true";
pub(crate) const KEYWORD_VAR: &str = "var";
pub(crate) const KEYWORD_VOID: &str = "void";
pub(crate) const KEYWORD_WHILE: &str = "while";

// Symbols
pub(crate) const SYMBOL_AMPERSAND: char = '&';
pub(crate) const SYMBOL_ASTERISK: char = '*';
pub(crate) const SYMBOL_BACKSLASH: char = '\\';
pub(crate) const SYMBOL_COMMA: char = ',';
pub(crate) const SYMBOL_SGL_QUOTE: char = '\'';
pub(crate) const SYMBOL_DBL_QUOTE: char = '"';
pub(crate) const SYMBOL_DOT: char = '.';
pub(crate) const SYMBOL_EQUAL: char = '=';
pub(crate) const SYMBOL_GREATER_THAN: char = '>';
pub(crate) const SYMBOL_LEFT_CURLY_BRACE: char = '{';
pub(crate) const SYMBOL_LEFT_PAREN: char = '(';
pub(crate) const SYMBOL_LEFT_SQUARE_BRACKET: char = '[';
pub(crate) const SYMBOL_LESS_THAN: char = '<';
pub(crate) const SYMBOL_MINUS: char = '-';
pub(crate) const SYMBOL_PLUS: char = '+';
pub(crate) const SYMBOL_RIGHT_CURLY_BRACE: char = '}';
pub(crate) const SYMBOL_RIGHT_PAREN: char = ')';
pub(crate) const SYMBOL_RIGHT_SQUARE_BRACKET: char = ']';
pub(crate) const SYMBOL_SEMICOLON: char = ';';
pub(crate) const SYMBOL_SLASH: char = '/';
pub(crate) const SYMBOL_TILDE: char = '~';
pub(crate) const SYMBOL_VERTICAL_BAR: char = '|';

////////////////////////////////////////////////////////////////////////////////
// Tokens, Keywords, and Symbols
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub(crate) enum Token {
  Identifier(String, usize),
  IntegerConstant(u16, usize),
  CharacterConstant(char, usize),
  Keyword(Keyword, usize),
  StringConstant(String, usize),
  Symbol(Symbol, usize),
}

impl Token {
  pub(crate) fn get_pos(&self) -> usize {
    return match self {
      Token::Identifier(_, pos) => *pos,
      Token::IntegerConstant(_, pos) => *pos,
      Token::CharacterConstant(_, pos) => *pos,
      Token::Keyword(_, pos) => *pos,
      Token::StringConstant(_, pos) => *pos,
      Token::Symbol(_, pos) => *pos,
    };
  }
}

impl PartialEq for Token {
  fn eq(&self, other: &Self) -> bool {
    return match (self, other) {
      (Token::Identifier(id1, _), Token::Identifier(id2, _)) => id1 == id2,
      (Token::IntegerConstant(num1, _), Token::IntegerConstant(num2, _)) => {
        num1 == num2
      }
      (Token::Keyword(kw1, _), Token::Keyword(kw2, _)) => kw1 == kw2,
      (Token::StringConstant(str1, _), Token::StringConstant(str2, _)) => {
        str1 == str2
      }
      (Token::Symbol(sym1, _), Token::Symbol(sym2, _)) => sym1 == sym2,
      _ => false,
    };
  }
}

impl Display for Token {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    return match self {
      Token::Identifier(id, _) => write!(f, "identifier `{}`", id),
      Token::IntegerConstant(num, _) => write!(f, "integer constant `{}`", num),
      Token::CharacterConstant(num, _) => {
        write!(f, "character constant `{}`", num)
      }
      Token::Keyword(kw, _) => write!(f, "keyword `{}`", kw),
      Token::StringConstant(str, _) => write!(f, "string constant `{}`", str),
      Token::Symbol(sym, _) => write!(f, "symbol `{}`", sym),
    };
  }
}

impl From<Token> for (Operator, usize) {
  fn from(val: Token) -> Self {
    return match val {
      Token::Symbol(Symbol::Amp, pos) => (Operator::And, pos),
      Token::Symbol(Symbol::Asterisk, pos) => (Operator::Mul, pos),
      Token::Symbol(Symbol::Eq, pos) => (Operator::Eq, pos),
      Token::Symbol(Symbol::Gt, pos) => (Operator::Gt, pos),
      Token::Symbol(Symbol::Lt, pos) => (Operator::Lt, pos),
      Token::Symbol(Symbol::Minus, pos) => (Operator::Sub, pos),
      Token::Symbol(Symbol::Plus, pos) => (Operator::Add, pos),
      Token::Symbol(Symbol::Slash, pos) => (Operator::Div, pos),
      Token::Symbol(Symbol::Tilde, pos) => (Operator::Not, pos),
      Token::Symbol(Symbol::VBar, pos) => (Operator::Or, pos),
      _ => error_panic!("Invalid conversion from token to operator"),
    };
  }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum Keyword {
  Any, // INFO: Not actually a jack keyword but necessary for type checking
  Boolean,
  Char,
  Class,
  Constructor,
  Do,
  Else,
  False,
  Field,
  Function,
  If,
  Int,
  Let,
  Method,
  Null,
  Return,
  Static,
  This,
  True,
  Var,
  Void,
  While,
}

impl Display for Keyword {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    return match self {
      Keyword::Any => write!(f, "any"),
      Keyword::Boolean => write!(f, "boolean"),
      Keyword::Char => write!(f, "char"),
      Keyword::Class => write!(f, "class"),
      Keyword::Constructor => write!(f, "constructor"),
      Keyword::Do => write!(f, "do"),
      Keyword::Else => write!(f, "else"),
      Keyword::False => write!(f, "false"),
      Keyword::Field => write!(f, "field"),
      Keyword::Function => write!(f, "function"),
      Keyword::If => write!(f, "if"),
      Keyword::Int => write!(f, "int"),
      Keyword::Let => write!(f, "let"),
      Keyword::Method => write!(f, "method"),
      Keyword::Null => write!(f, "null"),
      Keyword::Return => write!(f, "return"),
      Keyword::Static => write!(f, "static"),
      Keyword::This => write!(f, "this"),
      Keyword::True => write!(f, "true"),
      Keyword::Var => write!(f, "var"),
      Keyword::Void => write!(f, "void"),
      Keyword::While => write!(f, "while"),
    };
  }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Symbol {
  Amp,
  Asterisk,
  BlockComment,
  Comma,
  Dot,
  Eq,
  Gt,
  LeftCurlyBrace,
  LeftParen,
  LeftSquareBracket,
  Lt,
  LineComment,
  Minus,
  Plus,
  RightCurlyBrace,
  RightParen,
  RightSquareBracket,
  Semicolon,
  Slash,
  Tilde,
  VBar,
}

impl Symbol {
  pub(crate) fn is_operator(&self) -> bool {
    return matches!(
      self,
      Symbol::Asterisk
        | Symbol::Eq
        | Symbol::Gt
        | Symbol::Lt
        | Symbol::Minus
        | Symbol::Plus
        | Symbol::Slash
        | Symbol::Tilde
        | Symbol::VBar
        | Symbol::Amp
    );
  }
}

impl Display for Symbol {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    return match self {
      Symbol::Amp => write!(f, "&amp;"),
      Symbol::Asterisk => write!(f, "*"),
      Symbol::BlockComment => write!(f, "/*"),
      Symbol::Comma => write!(f, ","),
      Symbol::Dot => write!(f, "."),
      Symbol::Eq => write!(f, "="),
      Symbol::Gt => write!(f, "&gt;"),
      Symbol::LeftCurlyBrace => write!(f, "{{"),
      Symbol::LeftParen => write!(f, "("),
      Symbol::LeftSquareBracket => write!(f, "["),
      Symbol::Lt => write!(f, "&lt;"),
      Symbol::LineComment => write!(f, "//"),
      Symbol::Minus => write!(f, "-"),
      Symbol::Plus => write!(f, "+"),
      Symbol::RightCurlyBrace => write!(f, "}}"),
      Symbol::RightParen => write!(f, ")"),
      Symbol::RightSquareBracket => write!(f, "]"),
      Symbol::Semicolon => write!(f, ";"),
      Symbol::Slash => write!(f, "/"),
      Symbol::Tilde => write!(f, "~"),
      Symbol::VBar => write!(f, "|"),
    };
  }
}

////////////////////////////////////////////////////////////////////////////////
// Tokenizer
////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Debug)]
pub(crate) struct Tokenizer<'a> {
  pos: usize,
  file_info: &'a FileInfo,
  chars: Peekable<Chars<'a>>,
  current_token: Option<Token>,
  next_token: VecDeque<Option<Token>>,
  tokens: Vec<Token>,
}

impl<'a> Tokenizer<'a> {
  pub(crate) fn new(file_info: &'a FileInfo) -> Self {
    let mut _self = Self {
      pos: 0,
      file_info,
      chars: file_info.content.chars().peekable(),
      current_token: None,
      next_token: VecDeque::new(),
      tokens: Vec::new(),
    };

    _self.next();

    return _self;
  }

  pub(crate) fn get_tokens(&self) -> &Vec<Token> {
    return &self.tokens;
  }

  pub(crate) fn expect(&mut self, token: Token) -> usize {
    let next_token = self.next().expect(error_fmt_file!(
      &self.file_info.name,
      &self.file_info.content,
      self.pos,
      "(ParseError)",
      "Unexpected end of file"
    ));

    if next_token != token {
      error_panic_fmt_file!(
        &self.file_info.name,
        &self.file_info.content,
        next_token.get_pos(),
        "(ParseError)",
        "Expected token (`{}`). Found: ({}).",
        token,
        next_token
      );
    }

    return next_token.get_pos();
  }

  fn skip_comment(&mut self, comment_type: Symbol) {
    let start_pos = self.pos;

    match comment_type {
      Symbol::LineComment => {
        while self.chars.peek() != Some(&'\n') && self.chars.next().is_some() {
          self.pos += 1;
        }
      }
      Symbol::BlockComment => {
        let mut nested_block = 1;
        self.chars.next();
        self.pos += 1;

        while nested_block > 0 && self.pos + 2 < self.file_info.content.len() {
          let c_curr = self.chars.next().expect(error_fmt_file!(
            &self.file_info.name,
            &self.file_info.content,
            start_pos,
            "(ParseError)",
            "Unexpected end of block comment"
          ));
          let c_next = self.chars.peek().expect(error_fmt_file!(
            &self.file_info.name,
            &self.file_info.content,
            start_pos,
            "(ParseError)",
            "Unexpected end of block comment"
          ));

          match (c_curr, c_next) {
            (SYMBOL_ASTERISK, &SYMBOL_SLASH) => {
              if nested_block <= 0 {
                error_panic_fmt_file!(
                  &self.file_info.name,
                  &self.file_info.content,
                  self.pos,
                  "(ParseError)",
                  "Mismatched end of block comment"
                );
              }

              nested_block -= 1;
              self.chars.next();
              self.pos += 1;
            }
            (SYMBOL_SLASH, &SYMBOL_ASTERISK) => {
              nested_block += 1;
            }
            _ => {}
          }

          self.pos += 1;
        }

        if nested_block != 0 {
          error_panic_fmt_file!(
            &self.file_info.name,
            &self.file_info.content,
            start_pos,
            "(ParseError)",
            "Unterminated block comment"
          );
        }
      }
      _ => error_panic!("Invalid comment type"),
    }
  }

  fn parse_keyword_or_identifier(&mut self, start_char: char) -> Option<Token> {
    let start_pos = self.pos;
    let mut raw_token = String::from(start_char);

    while let Some(c) = self
      .chars
      .next_if(|c| return c.is_alphanumeric() || *c == '_')
    {
      self.pos += 1;
      raw_token.push(c);
    }

    return Some(match raw_token.as_str() {
      KEYWORD_CLASS => Token::Keyword(Keyword::Class, start_pos),
      KEYWORD_CONSTRUCTOR => Token::Keyword(Keyword::Constructor, start_pos),
      KEYWORD_FUNCTION => Token::Keyword(Keyword::Function, start_pos),
      KEYWORD_METHOD => Token::Keyword(Keyword::Method, start_pos),
      KEYWORD_FIELD => Token::Keyword(Keyword::Field, start_pos),
      KEYWORD_STATIC => Token::Keyword(Keyword::Static, start_pos),
      KEYWORD_VAR => Token::Keyword(Keyword::Var, start_pos),
      KEYWORD_INT => Token::Keyword(Keyword::Int, start_pos),
      KEYWORD_CHAR => Token::Keyword(Keyword::Char, start_pos),
      KEYWORD_BOOLEAN => Token::Keyword(Keyword::Boolean, start_pos),
      KEYWORD_VOID => Token::Keyword(Keyword::Void, start_pos),
      KEYWORD_TRUE => Token::Keyword(Keyword::True, start_pos),
      KEYWORD_FALSE => Token::Keyword(Keyword::False, start_pos),
      KEYWORD_NULL => Token::Keyword(Keyword::Null, start_pos),
      KEYWORD_THIS => Token::Keyword(Keyword::This, start_pos),
      KEYWORD_LET => Token::Keyword(Keyword::Let, start_pos),
      KEYWORD_DO => Token::Keyword(Keyword::Do, start_pos),
      KEYWORD_IF => Token::Keyword(Keyword::If, start_pos),
      KEYWORD_ELSE => Token::Keyword(Keyword::Else, start_pos),
      KEYWORD_WHILE => Token::Keyword(Keyword::While, start_pos),
      KEYWORD_RETURN => Token::Keyword(Keyword::Return, start_pos),
      _ => Token::Identifier(raw_token, start_pos),
    });
  }

  fn parse_numeric(&mut self, start_char: char) -> Option<Token> {
    let start_pos = self.pos;
    let mut number = String::from(start_char);

    while let Some(c) = self.chars.next_if(|c| return c.is_numeric()) {
      self.pos += 1;
      number.push(c);
    }

    let number = number.parse::<u16>().expect(error_fmt_file!(
      &self.file_info.name,
      &self.file_info.content,
      start_pos,
      "(ParseError)",
      "Integer literal is too large for 16-bit representation"
    ));

    return Some(Token::IntegerConstant(number, start_pos));
  }

  fn parse_symbol(&mut self, symbol: char) -> Option<Token> {
    return Some(match symbol {
      SYMBOL_AMPERSAND => Token::Symbol(Symbol::Amp, self.pos),
      SYMBOL_LEFT_PAREN => Token::Symbol(Symbol::LeftParen, self.pos),
      SYMBOL_RIGHT_PAREN => Token::Symbol(Symbol::RightParen, self.pos),
      SYMBOL_ASTERISK => Token::Symbol(Symbol::Asterisk, self.pos),
      SYMBOL_PLUS => Token::Symbol(Symbol::Plus, self.pos),
      SYMBOL_COMMA => Token::Symbol(Symbol::Comma, self.pos),
      SYMBOL_MINUS => Token::Symbol(Symbol::Minus, self.pos),
      SYMBOL_DOT => Token::Symbol(Symbol::Dot, self.pos),
      SYMBOL_SLASH => Token::Symbol(Symbol::Slash, self.pos),
      SYMBOL_SEMICOLON => Token::Symbol(Symbol::Semicolon, self.pos),
      SYMBOL_LESS_THAN => Token::Symbol(Symbol::Lt, self.pos),
      SYMBOL_EQUAL => Token::Symbol(Symbol::Eq, self.pos),
      SYMBOL_GREATER_THAN => Token::Symbol(Symbol::Gt, self.pos),
      SYMBOL_LEFT_SQUARE_BRACKET => {
        Token::Symbol(Symbol::LeftSquareBracket, self.pos)
      }
      SYMBOL_RIGHT_SQUARE_BRACKET => {
        Token::Symbol(Symbol::RightSquareBracket, self.pos)
      }
      SYMBOL_LEFT_CURLY_BRACE => {
        Token::Symbol(Symbol::LeftCurlyBrace, self.pos)
      }
      SYMBOL_VERTICAL_BAR => Token::Symbol(Symbol::VBar, self.pos),
      SYMBOL_RIGHT_CURLY_BRACE => {
        Token::Symbol(Symbol::RightCurlyBrace, self.pos)
      }
      SYMBOL_TILDE => Token::Symbol(Symbol::Tilde, self.pos),
      _ => error_panic_fmt_file!(
        &self.file_info.name,
        &self.file_info.content,
        self.pos,
        "(ParseError)",
        "Invalid symbol: `{}`.",
        symbol
      ),
    });
  }

  fn parse_char(&mut self) -> Option<Token> {
    let start_pos = self.pos;
    let mut assignment_counter = 0;
    let mut char = '\0';

    while let Some(c) = self.chars.next() {
      self.pos += 1;

      match c {
        SYMBOL_SGL_QUOTE => break,
        SYMBOL_BACKSLASH => {
          char = self.parse_escape_sequence();
          assignment_counter += 1;
          continue;
        }
        _ => {}
      }

      if !c.is_ascii() {
        error_panic_fmt_file!(
          &self.file_info.name,
          &self.file_info.content,
          start_pos,
          "(ParseError)",
          "Character literal must be an ASCII character."
        );
      }

      char = c;
      assignment_counter += 1;
    }

    if assignment_counter != 1 {
      error_panic_fmt_file!(
        &self.file_info.name,
        &self.file_info.content,
        start_pos,
        "(ParseError)",
        "Character literal must be exactly one character long."
      );
    }

    return Some(Token::CharacterConstant(char, start_pos));
  }

  fn parse_string(&mut self) -> Option<Token> {
    let start_pos = self.pos;
    let mut string = String::new();

    while let Some(c) = self.chars.next() {
      self.pos += 1;

      match c {
        SYMBOL_DBL_QUOTE => break,
        SYMBOL_BACKSLASH => {
          string.push(self.parse_escape_sequence());
          continue;
        }
        _ => {}
      }

      string.push(c);
    }

    return Some(Token::StringConstant(string, start_pos));
  }

  fn parse_escape_sequence(&mut self) -> char {
    return match self.chars.next() {
      Some('n') => 128 as char,
      Some('r') => '\r',
      Some('b') => 129 as char,
      Some('t') => '\t',
      Some('\'') => '\'',
      Some('"') => '"',
      Some('\\') => '\\',
      Some('0') => '\0',
      Some(c) => {
        error_panic_fmt_file!(
          &self.file_info.name,
          &self.file_info.content,
          self.pos,
          "(ParseError)",
          "Invalid escape sequence `\\{}`.",
          c
        );
      }
      None => {
        error_panic_fmt_file!(
          &self.file_info.name,
          &self.file_info.content,
          self.pos,
          "(ParseError)",
          "Unexpected end of file."
        );
      }
    };
  }

  fn push_token(&mut self, token: Option<Token>) -> Option<Token> {
    if let Some(token) = token.clone() {
      self.tokens.push(token.clone());
    }

    return token;
  }

  fn advance(&mut self) -> Option<Token> {
    while let Some(c_curr) = self.chars.next() {
      if c_curr.is_whitespace() {
        self.pos += 1;
        continue;
      }

      self.pos += 1;

      let c_next = self.chars.peek().cloned();

      match (c_curr, c_next) {
        (SYMBOL_SLASH, Some(SYMBOL_SLASH)) => {
          self.skip_comment(Symbol::LineComment);
          continue;
        }
        (SYMBOL_SLASH, Some(SYMBOL_ASTERISK)) => {
          self.skip_comment(Symbol::BlockComment);
          continue;
        }
        (SYMBOL_SGL_QUOTE, _) => {
          let char = self.parse_char();
          return self.push_token(char);
        }
        (SYMBOL_DBL_QUOTE, _) => {
          let string = self.parse_string();
          return self.push_token(string);
        }
        _ => {}
      }

      if c_curr.is_numeric() {
        let number = self.parse_numeric(c_curr);
        return self.push_token(number);
      }

      if c_curr.is_alphanumeric() || c_curr == '_' {
        let alphanumeric = self.parse_keyword_or_identifier(c_curr);
        return self.push_token(alphanumeric);
      }

      let symbol = self.parse_symbol(c_curr);
      return self.push_token(symbol);
    }

    return None;
  }

  pub(crate) fn peek(&mut self, depth: usize) -> Option<Token> {
    if self.next_token.len() <= depth {
      let mut token = None;

      while self.next_token.len() <= depth {
        token = self.advance();
        self.next_token.push_back(token.clone());
      }

      return token;
    }

    return self
      .next_token
      .get(depth)
      .expect(error_fmt_file!(
        &self.file_info.name,
        &self.file_info.content,
        self.pos,
        "(ParseError)",
        "Unexpected end of file."
      ))
      .clone();
  }
}

impl<'a> Iterator for Tokenizer<'a> {
  type Item = Token;
  fn next(&mut self) -> Option<Self::Item> {
    self
      .current_token
      .clone_from(&self.next_token.pop_front().unwrap_or(None));

    if self.next_token.is_empty() {
      let t = self.advance();
      self.next_token.push_back(t);
    }

    return self.current_token.take();
  }
}
