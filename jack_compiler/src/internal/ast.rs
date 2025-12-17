////////////////////////////////////////////////////////////////////////////////
// File: src/internal/ast.rs
// Description: AST (Abstract Syntax Tree) definition
//
// Author: Leon Heidelbach <leon.heidelbach@hhu.de>
// Date: 15.05.2024
//
// License: GPLv3
////////////////////////////////////////////////////////////////////////////////

use std::{
  cell::RefCell,
  fmt::{Display, Formatter},
};

use super::{
  jack::{JackToken, SymbolKind},
  tokenize::{
    Keyword, Symbol, Token, Tokenizer, KEYWORD_ELSE, KEYWORD_IF, KEYWORD_WHILE,
  },
  types::{BUILTIN_TYPE_ARRAY, BUILTIN_TYPE_NULL, BUILTIN_TYPE_STRING},
};

use anyhow::Result;
use shared::{
  error_fmt_file, error_fmt_src, error_panic_fmt_file, error_unreachable,
  io::os::FileInfo, warning_println_file,
};

////////////////////////////////////////////////////////////////////////////////
// Common traits
////////////////////////////////////////////////////////////////////////////////

trait FromTokenizer {
  fn from_tokenizer(
    file_info: &FileInfo,
    scope_gen: &mut ScopeGenerator,
    tokenizer: &mut Tokenizer,
    pos: usize,
  ) -> Self;
}

////////////////////////////////////////////////////////////////////////////////
// Scopes
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub(crate) struct ScopeInfo {
  pub(crate) class_name: String,
  pub(crate) subroutine_type: Keyword,
  pub(crate) subroutine_name: String,
  pub(crate) sub_scope: String,
}

impl ScopeInfo {
  pub(crate) fn get_base_scope(&self) -> String {
    if self.subroutine_name.is_empty() {
      return self.class_name.clone();
    } else {
      return format!("{}.{}", self.class_name, self.subroutine_name);
    }
  }
}

impl Display for ScopeInfo {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    let class_is_empty = self.class_name.is_empty();
    let function_is_empty = self.subroutine_name.is_empty();
    let sub_scope_is_empty = self.sub_scope.is_empty();

    if class_is_empty && function_is_empty && sub_scope_is_empty {
      error_unreachable!("No class name, function name or sub scope found.");
    } else if !class_is_empty && !function_is_empty && sub_scope_is_empty {
      return write!(f, "{}.{}", self.class_name, self.subroutine_name);
    } else if !class_is_empty && function_is_empty && !sub_scope_is_empty {
      error_unreachable!("No function name found but sub scope was defined.");
    } else if !class_is_empty && !function_is_empty && !sub_scope_is_empty {
      return write!(
        f,
        "{}.{}{}",
        self.class_name, self.subroutine_name, self.sub_scope
      );
    } else {
      return write!(f, "{}", self.class_name);
    }
  }
}

pub(crate) struct ScopeGenerator {
  pub(crate) scope_depth: usize,
  pub(crate) class_name: Option<Identifier>,
  pub(crate) subroutine_type: Option<Keyword>,
  pub(crate) subroutine_name: Option<Identifier>,
  pub(crate) sub_scope: Vec<String>,
}

impl ScopeGenerator {
  pub(crate) fn set_class(&mut self, class: &Identifier) {
    self.class_name = Some(class.clone());
  }

  pub(crate) fn get_class(&self) -> Option<&Identifier> {
    return self.class_name.as_ref();
  }

  pub(crate) fn set_function(
    &mut self,
    function_type: &Keyword,
    ident: &Identifier,
  ) {
    self.subroutine_type = Some(*function_type);
    self.subroutine_name = Some(ident.clone());
    self.sub_scope.clear();
  }

  pub(crate) fn get_subroutine_type(&self) -> Option<&Keyword> {
    return self.subroutine_type.as_ref();
  }

  pub(crate) fn push_sub_scope(&mut self, sub_scope: &str) {
    self.scope_depth += 1;
    self
      .sub_scope
      .push(format!(" > {}_{}", sub_scope, self.scope_depth));
  }

  pub(crate) fn pop_sub_scope(&mut self) {
    self.sub_scope.pop();
  }

  pub(crate) fn get_sub_scope(&self) -> String {
    return self.sub_scope.join(" ");
  }

  pub(crate) fn create_scope_info(&self) -> ScopeInfo {
    let class_name = if let Some(class_name) = self.class_name.as_ref() {
      class_name.name.clone()
    } else {
      String::new()
    };

    let subroutine_type =
      if let Some(subroutine_type) = self.subroutine_type.as_ref() {
        *subroutine_type
      } else {
        Keyword::Void
      };

    let subroutine_name =
      if let Some(subroutine_name) = self.subroutine_name.as_ref() {
        subroutine_name.name.clone()
      } else {
        String::new()
      };

    return ScopeInfo {
      class_name,
      subroutine_type,
      subroutine_name,
      sub_scope: self.get_sub_scope(),
    };
  }
}

impl Default for ScopeGenerator {
  fn default() -> Self {
    return Self {
      scope_depth: 0,
      class_name: None,
      subroutine_type: None,
      subroutine_name: None,
      sub_scope: Vec::new(),
    };
  }
}

////////////////////////////////////////////////////////////////////////////////
// Identifiers & Literals
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub(crate) struct Identifier {
  pub(crate) pos: usize,
  pub(crate) scope: ScopeInfo,
  pub(crate) name: String,
  pub(crate) is_class_var: bool,
  pub(crate) array_offset: Option<Box<ExpressionInfo>>,
}

impl FromTokenizer for Identifier {
  fn from_tokenizer(
    file_info: &FileInfo,
    scope_gen: &mut ScopeGenerator,
    tokenizer: &mut Tokenizer,
    pos: usize,
  ) -> Self {
    let scope = scope_gen.create_scope_info();

    let Some(ident) = tokenizer.next() else {
      error_panic_fmt_file!(
        &file_info.name,
        &file_info.content,
        pos,
        "(ParseError)",
        "Expected (´identifier´). Found: (EOF)."
      )
    };

    let Token::Identifier(name, pos) = ident else {
      error_panic_fmt_file!(
        &file_info.name,
        &file_info.content,
        ident.get_pos(),
        "(ParseError)",
        "Expected (´identifier´). Found: ({}).",
        ident
      )
    };

    let array_offset = match tokenizer.peek(0) {
      Some(Token::Symbol(Symbol::LeftSquareBracket, _)) => {
        tokenizer.next();
        let offset = Ast::parse_expression(file_info, scope_gen, tokenizer);
        tokenizer.expect(Token::Symbol(Symbol::RightSquareBracket, 0));
        Some(Box::new(offset))
      }
      _ => None,
    };

    return Self {
      pos,
      scope,
      name,
      is_class_var: false,
      array_offset,
    };
  }
}

#[derive(Debug, Clone)]
pub(crate) struct Integer {
  pub(crate) pos: usize,
  pub(crate) value: u16,
}

impl FromTokenizer for Integer {
  fn from_tokenizer(
    file_info: &FileInfo,
    _scope_gen: &mut ScopeGenerator,
    tokenizer: &mut Tokenizer,
    pos: usize,
  ) -> Self {
    let Some(int) = tokenizer.next() else {
      error_panic_fmt_file!(
        &file_info.name,
        &file_info.content,
        pos,
        "(ParseError)",
        "Expected (´int´). Found: (EOF)."
      )
    };

    let Token::IntegerConstant(value, pos) = int else {
      error_panic_fmt_file!(
        &file_info.name,
        &file_info.content,
        int.get_pos(),
        "(ParseError)",
        "Expected (´int´). Found: ({}).",
        int
      )
    };

    return Self { pos, value };
  }
}

#[derive(Debug, Clone)]
pub(crate) struct Character {
  pub(crate) pos: usize,
  pub(crate) character: char,
  pub(crate) value: u16,
}

impl FromTokenizer for Character {
  fn from_tokenizer(
    file_info: &FileInfo,
    _scope_gen: &mut ScopeGenerator,
    tokenizer: &mut Tokenizer,
    pos: usize,
  ) -> Self {
    let Some(char) = tokenizer.next() else {
      error_panic_fmt_file!(
        &file_info.name,
        &file_info.content,
        pos,
        "(ParseError)",
        "Expected (´int´). Found: (EOF)."
      )
    };

    let Token::CharacterConstant(value, pos) = char else {
      error_panic_fmt_file!(
        &file_info.name,
        &file_info.content,
        char.get_pos(),
        "(ParseError)",
        "Expected (´char´). Found: ({}).",
        char
      )
    };

    return Self {
      pos,
      character: value,
      value: value as u16,
    };
  }
}

#[derive(Debug, Clone)]
pub(crate) struct Boolean {
  pub(crate) pos: usize,
  pub(crate) value: bool,
}

impl FromTokenizer for Boolean {
  fn from_tokenizer(
    file_info: &FileInfo,
    _scope_gen: &mut ScopeGenerator,
    tokenizer: &mut Tokenizer,
    pos: usize,
  ) -> Self {
    let Some(boolean) = tokenizer.next() else {
      error_panic_fmt_file!(
        &file_info.name,
        &file_info.content,
        pos,
        "(ParseError)",
        "Expected (´boolean´). Found: (EOF)."
      )
    };

    let Token::Keyword(ref value, pos) = boolean else {
      error_panic_fmt_file!(
        &file_info.name,
        &file_info.content,
        boolean.get_pos(),
        "(ParseError)",
        "Expected (´boolean´). Found: ({}).",
        boolean
      )
    };

    let value = match value {
      Keyword::True => true,
      Keyword::False => false,
      _ => error_panic_fmt_file!(
        &file_info.name,
        &file_info.content,
        pos,
        "(ParseError)",
        "Expected (´boolean´). Found: ({}).",
        boolean
      ),
    };

    return Self { pos, value };
  }
}

#[derive(Debug, Clone)]
pub(crate) struct SizedString {
  pub(crate) pos: usize,
  pub(crate) value: String,
}

impl FromTokenizer for SizedString {
  fn from_tokenizer(
    file_info: &FileInfo,
    _scope_gen: &mut ScopeGenerator,
    tokenizer: &mut Tokenizer,
    pos: usize,
  ) -> Self {
    let Some(str) = tokenizer.next() else {
      error_panic_fmt_file!(
        &file_info.name,
        &file_info.content,
        pos,
        "(ParseError)",
        "Expected (´string´). Found: (EOF)."
      )
    };

    let Token::StringConstant(value, pos) = str else {
      error_panic_fmt_file!(
        &file_info.name,
        &file_info.content,
        str.get_pos(),
        "(ParseError)",
        "Expected (´string´). Found: ({}).",
        str
      )
    };

    return Self { pos, value };
  }
}

#[derive(Debug, Clone)]
pub(crate) struct Null {
  pub(crate) pos: usize,
}

impl FromTokenizer for Null {
  fn from_tokenizer(
    _file_info: &FileInfo,
    _scope_gen: &mut ScopeGenerator,
    tokenizer: &mut Tokenizer,
    pos: usize,
  ) -> Self {
    tokenizer.expect(Token::Keyword(Keyword::Null, 0));

    return Self { pos };
  }
}

#[derive(Debug, Clone)]
pub(crate) struct This {
  pub(crate) pos: usize,
  pub(crate) scope: ScopeInfo,
}

impl FromTokenizer for This {
  fn from_tokenizer(
    file_info: &FileInfo,
    scope_gen: &mut ScopeGenerator,
    tokenizer: &mut Tokenizer,
    pos: usize,
  ) -> Self {
    let scope = scope_gen.create_scope_info();

    tokenizer.expect(Token::Keyword(Keyword::This, 0));

    let function_type = scope_gen.get_subroutine_type().expect(error_fmt_src!(
      "Tokenizer could not provide the current function name."
    ));

    if *function_type == Keyword::Function {
      error_panic_fmt_file!(
        &file_info.name,
        &file_info.content,
        pos,
        "(ParseError)",
        "Found keyword `{}` in {} scope `{}`. Cannot reference \
            object instance in static functions.",
        Keyword::This,
        Keyword::Function,
        scope
      )
    }

    return Self { pos, scope };
  }
}

////////////////////////////////////////////////////////////////////////////////
// Operators
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Operator {
  Mul,
  Div,
  Add,
  Sub,
  Not,
  Eq,
  Lt,
  Gt,
  And,
  Or,
}

impl Operator {
  pub(crate) fn is_comparison_op(&self) -> bool {
    match self {
      Operator::Eq | Operator::Lt | Operator::Gt => return true,
      _ => return false,
    }
  }

  pub(crate) fn is_valid_unary_op(&self) -> bool {
    match self {
      Operator::Add | Operator::Sub | Operator::Not => return true,
      _ => return false,
    }
  }

  pub(crate) fn is_valid_boolean_op(&self) -> bool {
    match self {
      Operator::Not | Operator::And | Operator::Or | Operator::Eq => {
        return true
      }
      _ => return false,
    }
  }

  pub(crate) fn is_valid_string_op(&self) -> bool {
    match self {
      Operator::Eq => return true,
      _ => return false,
    }
  }
}

impl Display for Operator {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      Operator::Mul => return write!(f, "*"),
      Operator::Div => return write!(f, "/"),
      Operator::Add => return write!(f, "+"),
      Operator::Sub => return write!(f, "-"),
      Operator::Not => return write!(f, "~"),
      Operator::Eq => return write!(f, "="),
      Operator::Lt => return write!(f, "<"),
      Operator::Gt => return write!(f, ">"),
      Operator::And => return write!(f, "&"),
      Operator::Or => return write!(f, "|"),
    }
  }
}

////////////////////////////////////////////////////////////////////////////////
// Statements
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub(crate) struct Block {
  pub(crate) statements: Vec<Statement>,
}

impl FromTokenizer for Block {
  fn from_tokenizer(
    file_info: &FileInfo,
    scope_gen: &mut ScopeGenerator,
    tokenizer: &mut Tokenizer,
    _pos: usize,
  ) -> Self {
    let mut statements = Vec::new();

    tokenizer.expect(Token::Symbol(Symbol::LeftCurlyBrace, 0));

    while let Some(token) = tokenizer.peek(0) {
      match token {
        Token::Keyword(Keyword::Var, pos) => statements.push(
          Statement::VariableDeclaration(VariableDeclaration::from_tokenizer(
            file_info, scope_gen, tokenizer, pos,
          )),
        ),
        Token::Keyword(Keyword::Let, pos) => statements.push(
          Statement::VariableAssignment(VariableAssignment::from_tokenizer(
            file_info, scope_gen, tokenizer, pos,
          )),
        ),
        Token::Keyword(Keyword::If, pos) => statements.push(Statement::If(
          IfStatement::from_tokenizer(file_info, scope_gen, tokenizer, pos),
        )),
        Token::Keyword(Keyword::While, pos) => {
          statements.push(Statement::While(WhileStatement::from_tokenizer(
            file_info, scope_gen, tokenizer, pos,
          )))
        }
        Token::Keyword(Keyword::Do, pos) => {
          statements.push(Statement::SubroutineCall(
            SubroutineCall::from_tokenizer(
              file_info, scope_gen, tokenizer, pos,
            ),
          ));
          tokenizer.expect(Token::Symbol(Symbol::Semicolon, 0));
        }
        Token::Keyword(Keyword::Return, pos) => {
          statements.push(Statement::Return(ReturnStatement::from_tokenizer(
            file_info, scope_gen, tokenizer, pos,
          )))
        }
        Token::Symbol(Symbol::RightCurlyBrace, _) => {
          tokenizer.next();
          break;
        }
        _ => error_panic_fmt_file!(
          &file_info.name,
          &file_info.content,
          token.get_pos(),
          "(ParseError)",
          "Unexpected token in block. Expected (`{}`, `{}`, `{}`, \
              `{}`, `{}`, `{}` or `{}`). Found: ({}).",
          Keyword::Var,
          Keyword::Let,
          Keyword::If,
          Keyword::While,
          Keyword::Do,
          Keyword::Return,
          Symbol::RightCurlyBrace,
          token
        ),
      }
    }

    return Self { statements };
  }
}

#[derive(Debug, Clone)]
pub(crate) enum Statement {
  SubroutineCall(SubroutineCall),
  If(IfStatement),
  Return(ReturnStatement),
  VariableAssignment(VariableAssignment),
  VariableDeclaration(VariableDeclaration),
  While(WhileStatement),
}

#[derive(Debug, Clone)]
pub(crate) struct IfStatement {
  pub(crate) pos: usize,
  pub(crate) scope: ScopeInfo,
  pub(crate) scope_if: ScopeInfo,
  pub(crate) scope_else: ScopeInfo,
  pub(crate) condition: ExpressionInfo,
  pub(crate) if_body: Block,
  pub(crate) else_body: Option<Block>,
}

impl FromTokenizer for IfStatement {
  fn from_tokenizer(
    file_info: &FileInfo,
    scope_gen: &mut ScopeGenerator,
    tokenizer: &mut Tokenizer,
    pos: usize,
  ) -> Self {
    let scope = scope_gen.create_scope_info();

    tokenizer.expect(Token::Keyword(Keyword::If, 0));
    tokenizer.expect(Token::Symbol(Symbol::LeftParen, 0));

    let condition = Ast::parse_expression(file_info, scope_gen, tokenizer);

    tokenizer.expect(Token::Symbol(Symbol::RightParen, 0));

    scope_gen.push_sub_scope(KEYWORD_IF);

    let scope_if = scope_gen.create_scope_info();

    let if_body = Block::from_tokenizer(file_info, scope_gen, tokenizer, pos);

    scope_gen.pop_sub_scope();
    scope_gen.push_sub_scope(KEYWORD_ELSE);

    let scope_else = scope_gen.create_scope_info();

    let else_body: Option<Block> = if let Some(token) = tokenizer.peek(0) {
      match token {
        Token::Keyword(Keyword::Else, pos) => {
          tokenizer.next();
          Some(Block::from_tokenizer(file_info, scope_gen, tokenizer, pos))
        }
        _ => None,
      }
    } else {
      error_panic_fmt_file!(
        &file_info.name,
        &file_info.content,
        pos,
        "(ParseError)",
        "Expected (´else´ or `statement`). Found: (EOF)."
      )
    };

    scope_gen.pop_sub_scope();

    return Self {
      pos,
      scope,
      scope_if,
      scope_else,
      condition,
      if_body,
      else_body,
    };
  }
}

#[derive(Debug, Clone)]
pub(crate) struct WhileStatement {
  pub(crate) scope: ScopeInfo,
  pub(crate) scope_while: ScopeInfo,
  pub(crate) condition: ExpressionInfo,
  pub(crate) body: Block,
}

impl FromTokenizer for WhileStatement {
  fn from_tokenizer(
    file_info: &FileInfo,
    scope_gen: &mut ScopeGenerator,
    tokenizer: &mut Tokenizer,
    pos: usize,
  ) -> Self {
    let scope = scope_gen.create_scope_info();

    tokenizer.expect(Token::Keyword(Keyword::While, 0));
    tokenizer.expect(Token::Symbol(Symbol::LeftParen, 0));

    let condition = Ast::parse_expression(file_info, scope_gen, tokenizer);

    scope_gen.push_sub_scope(KEYWORD_WHILE);

    let scope_while = scope_gen.create_scope_info();

    tokenizer.expect(Token::Symbol(Symbol::RightParen, 0));

    let body = Block::from_tokenizer(file_info, scope_gen, tokenizer, pos);

    scope_gen.pop_sub_scope();

    return Self {
      scope,
      scope_while,
      condition,
      body,
    };
  }
}

#[derive(Debug, Clone)]
pub(crate) struct ReturnStatement {
  pub(crate) pos: usize,
  pub(crate) scope: ScopeInfo,
  pub(crate) value: Option<ExpressionInfo>,
}

impl FromTokenizer for ReturnStatement {
  fn from_tokenizer(
    file_info: &FileInfo,
    scope_gen: &mut ScopeGenerator,
    tokenizer: &mut Tokenizer,
    pos: usize,
  ) -> Self {
    let scope = scope_gen.create_scope_info();

    tokenizer.expect(Token::Keyword(Keyword::Return, 0));

    let value = if let Some(token) = tokenizer.peek(0) {
      match token {
        Token::Symbol(Symbol::Semicolon, _) => None,
        _ => Some(Ast::parse_expression(file_info, scope_gen, tokenizer)),
      }
    } else {
      error_panic_fmt_file!(
        &file_info.name,
        &file_info.content,
        pos,
        "(ParseError)",
        "Expected (´expression´ or ´;´). Found: (EOF)."
      )
    };

    tokenizer.expect(Token::Symbol(Symbol::Semicolon, 0));

    return Self { pos, scope, value };
  }
}

////////////////////////////////////////////////////////////////////////////////
// Types & Coercion
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Type {
  Integer,
  Character,
  Boolean,
  Void,
  AnyComplex,
  Complex(String),
}

impl Display for Type {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      Type::Integer => return write!(f, "int"),
      Type::Character => return write!(f, "char"),
      Type::Boolean => return write!(f, "boolean"),
      Type::Void => return write!(f, "void"),
      Type::AnyComplex => return write!(f, "any"),
      Type::Complex(ref name) => return write!(f, "{}", name),
    }
  }
}

impl Type {
  fn parse(raw_type: &Token) -> Option<Self> {
    match raw_type {
      Token::Keyword(raw_type, _) => match raw_type {
        Keyword::Int => return Some(Type::Integer),
        Keyword::Char => return Some(Type::Character),
        Keyword::Boolean => return Some(Type::Boolean),
        Keyword::Void => return Some(Type::Void),
        _ => return None,
      },
      Token::Identifier(raw_type, _) => {
        return Some(Self::Complex(raw_type.to_string()))
      }
      _ => return None,
    }
  }

  pub(crate) fn is_coerceable<'a>(
    &'a self,
    other: &'a Type,
  ) -> CoercionResult<'a> {
    let mut result = CoercionResult {
      success: false,
      from: self,
      to: other,
      warn: false,
    };

    match self {
      Type::Integer => match other {
        Type::Integer | Type::Character | Type::Boolean => {
          result.success = true;
          result.warn = self != other;
        }
        _ => match other {
          Type::Complex(ref name) => {
            result.success = name == BUILTIN_TYPE_ARRAY;
          }
          _ => (),
        },
      },
      Type::Boolean => match other {
        Type::Boolean | Type::Character | Type::Integer => {
          result.success = true;
          result.warn = self != other;
        }
        _ => match other {
          Type::Complex(ref name) => {
            result.success = name == BUILTIN_TYPE_ARRAY;
          }
          _ => (),
        },
      },
      Type::Character => match other {
        Type::Character | Type::Boolean | Type::Integer => {
          result.success = true;
          result.warn = self != other;
        }
        _ => match other {
          Type::Complex(ref name) => {
            result.success = name == BUILTIN_TYPE_ARRAY;
          }
          _ => (),
        },
      },
      Type::Complex(ref name) => {
        if name == BUILTIN_TYPE_NULL && other.is_complex()
          || other == &Type::Complex(BUILTIN_TYPE_NULL.to_string())
          || other == &Type::Complex(BUILTIN_TYPE_ARRAY.to_string())
          || other == &Type::AnyComplex
        {
          result.success = true;
          return result;
        }

        match name.as_str() {
          BUILTIN_TYPE_ARRAY => {
            result.success = true;
          }
          _ => {
            result.success = *self == *other;
          }
        }
      }
      Type::AnyComplex => {
        result.success = other.is_complex();
      }
      _ => (),
    }

    return result;
  }

  pub(crate) fn is_complex(&self) -> bool {
    match self {
      Type::Complex(_) => return true,
      _ => return false,
    }
  }

  pub(crate) fn is_builtin(&self) -> bool {
    match self {
      Type::Integer | Type::Character | Type::Boolean | Type::Void => {
        return true
      }
      Type::Complex(ref name) => match name.as_str() {
        BUILTIN_TYPE_STRING | BUILTIN_TYPE_ARRAY => return true,
        _ => return false,
      },
      _ => return false,
    }
  }
}

pub(crate) struct CoercionResult<'a> {
  success: bool,
  from: &'a Type,
  to: &'a Type,
  warn: bool,
}

impl<'a> CoercionResult<'a> {
  pub(crate) fn safe_unwrap(
    &self,
    file_info: &'a FileInfo,
    pos: usize,
  ) -> bool {
    if self.warn {
      warning_println_file!(
        &file_info.name,
        &file_info.content,
        pos,
        "(TypeChecker)",
        "Implicit type coercion from `{}` to `{}`.",
        self.from,
        self.to
      );
    }

    return self.success;
  }
}

////////////////////////////////////////////////////////////////////////////////
// Class, Class Variable, Subroutine, Parameter, Variable & Type Declarations
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub(crate) struct ClassDeclaration {
  pub(crate) pos: usize,
  pub(crate) identifier: Identifier,
  pub(crate) class_vars: Vec<ClassVariableDeclaration>,
  pub(crate) subroutines: Vec<SubroutineDeclaration>,
}

impl FromTokenizer for ClassDeclaration {
  fn from_tokenizer(
    file_info: &FileInfo,
    scope_gen: &mut ScopeGenerator,
    tokenizer: &mut Tokenizer,
    pos: usize,
  ) -> Self {
    let mut functions = Vec::new();
    let mut class_vars = Vec::new();

    tokenizer.expect(Token::Keyword(Keyword::Class, 0));

    let identifier =
      Identifier::from_tokenizer(file_info, scope_gen, tokenizer, pos);

    scope_gen.set_class(&identifier);

    tokenizer.expect(Token::Symbol(Symbol::LeftCurlyBrace, 0));

    while let Some(token) = tokenizer.peek(0) {
      match token {
        Token::Keyword(Keyword::Field, pos)
        | Token::Keyword(Keyword::Static, pos) => {
          class_vars.push(ClassVariableDeclaration::from_tokenizer(
            file_info, scope_gen, tokenizer, pos,
          ))
        }
        Token::Keyword(Keyword::Constructor, pos)
        | Token::Keyword(Keyword::Function, pos)
        | Token::Keyword(Keyword::Method, pos) => {
          functions.push(SubroutineDeclaration::from_tokenizer(
            file_info, scope_gen, tokenizer, pos,
          ))
        }
        Token::Symbol(Symbol::RightCurlyBrace, _) => {
          tokenizer.next();
          break;
        }
        _ => error_panic_fmt_file!(
          &file_info.name,
          &file_info.content,
          token.get_pos(),
          "(ParseError)",
          "Unexpected token in class body. Expected (`{}`, \
            `{}`, `{}`, `{}` or EOF). Found: ({}).",
          Keyword::Field,
          Keyword::Constructor,
          Keyword::Function,
          Keyword::Method,
          token
        ),
      }
    }

    return Self {
      pos,
      identifier,
      class_vars,
      subroutines: functions,
    };
  }
}

#[derive(Debug, Clone)]
pub(crate) struct ClassVariableDeclaration {
  pub(crate) var_type: SymbolKind,
  pub(crate) identifiers: Vec<Identifier>,
  pub(crate) type_dec: TypeDeclaration,
}

impl FromTokenizer for ClassVariableDeclaration {
  fn from_tokenizer(
    file_info: &FileInfo,
    scope_gen: &mut ScopeGenerator,
    tokenizer: &mut Tokenizer,
    pos: usize,
  ) -> Self {
    let mut identifiers = Vec::new();

    let var_type = tokenizer.next().expect(error_fmt_file!(
      &file_info.name,
      &file_info.content,
      pos,
      "(ParseError)",
      "Expected (`type declaration`). Found: (EOF)."
    ));

    let Token::Keyword(var_type, _) = var_type else {
      error_panic_fmt_file!(
        &file_info.name,
        &file_info.content,
        var_type.get_pos(),
        "(ParseError)",
        "Expected (keyword token: `{}`, `{}`, `{}` or \
          `class type`). Found: ({}).",
        Keyword::Int,
        Keyword::Boolean,
        Keyword::Char,
        var_type
      )
    };

    let type_dec = tokenizer.next().expect(error_fmt_file!(
      &file_info.name,
      &file_info.content,
      pos,
      "(ParseError)",
      "Expected (`type declaration`). Found (EOF)."
    ));

    let type_dec = match type_dec {
      Token::Identifier(_, pos) | Token::Keyword(_, pos) => TypeDeclaration {
        pos,
        raw_type: Type::parse(&type_dec).expect(error_fmt_file!(
          &file_info.name,
          &file_info.content,
          type_dec.get_pos(),
          "(ParseError)",
          "Invalid type found in field declaration: `{}`.",
          type_dec
        )),
      },
      _ => error_panic_fmt_file!(
        &file_info.name,
        &file_info.content,
        type_dec.get_pos(),
        "(ParseError)",
        "Invalid token type in field declaration: `{}`.",
        type_dec
      ),
    };

    while let Some(token) = tokenizer.peek(0) {
      match token {
        Token::Symbol(Symbol::Comma, _) => {
          tokenizer.next();
        }
        Token::Symbol(Symbol::Semicolon, _) => {
          tokenizer.next();
          break;
        }
        Token::Identifier(_, pos) => {
          identifiers.push(Identifier::from_tokenizer(
            file_info, scope_gen, tokenizer, pos,
          ));
        }
        _ => error_panic_fmt_file!(
          &file_info.name,
          &file_info.content,
          token.get_pos(),
          "(ParseError)",
          "Unexpected token in field declaration. Expected \
            (`identifier`, `{}` or `{}`). Found: ({}).",
          Symbol::Comma,
          Symbol::Semicolon,
          token
        ),
      }
    }

    return Self {
      var_type: var_type.into(),
      identifiers,
      type_dec,
    };
  }
}

#[derive(Debug, Clone)]
pub(crate) struct VariableDeclaration {
  pub(crate) scope: ScopeInfo,
  pub(crate) identifiers: Vec<Identifier>,
  pub(crate) type_dec: TypeDeclaration,
}

impl FromTokenizer for VariableDeclaration {
  fn from_tokenizer(
    file_info: &FileInfo,
    scope_gen: &mut ScopeGenerator,
    tokenizer: &mut Tokenizer,
    pos: usize,
  ) -> Self {
    let scope = scope_gen.create_scope_info();

    tokenizer.expect(Token::Keyword(Keyword::Var, 0));

    let mut identifiers = Vec::new();

    let r#type = tokenizer.next().expect(error_fmt_file!(
      &file_info.name,
      &file_info.content,
      pos,
      "(ParseError)",
      "Expected (`type declaration`). Found (EOF)."
    ));

    let r#type = match r#type {
      Token::Identifier(_, pos) | Token::Keyword(_, pos) => TypeDeclaration {
        pos,
        raw_type: Type::parse(&r#type).expect(error_fmt_file!(
          &file_info.name,
          &file_info.content,
          pos,
          "(ParseError)",
          "Invalid type found in variable declaration: `{}`.",
          r#type
        )),
      },
      _ => error_panic_fmt_file!(
        &file_info.name,
        &file_info.content,
        r#type.get_pos(),
        "(ParseError)",
        "Invalid token type in variable declaration: `{}`.",
        r#type
      ),
    };

    while let Some(token) = tokenizer.peek(0) {
      match token {
        Token::Symbol(Symbol::Comma, _) => {
          tokenizer.next();
        }
        Token::Symbol(Symbol::Semicolon, _) => {
          tokenizer.next();
          break;
        }
        Token::Identifier(_, pos) => {
          identifiers.push(Identifier::from_tokenizer(
            file_info, scope_gen, tokenizer, pos,
          ));
        }
        _ => error_panic_fmt_file!(
          &file_info.name,
          &file_info.content,
          token.get_pos(),
          "(ParseError)",
          "Unexpected token in variable declaration. Expected \
            (`identifier`, `{}` or `{}`). Found: ({}).",
          Symbol::Comma,
          Symbol::Semicolon,
          token
        ),
      }
    }

    if identifiers.is_empty() {
      error_panic_fmt_file!(
        &file_info.name,
        &file_info.content,
        pos,
        "(ParseError)",
        "Expected at least one identifier in variable declaration."
      )
    }

    return Self {
      scope,
      identifiers,
      type_dec: r#type,
    };
  }
}

#[derive(Debug, Clone)]
pub(crate) struct SubroutineDeclaration {
  pub(crate) pos: usize,
  pub(crate) scope: ScopeInfo,
  pub(crate) subroutine_type: Keyword,
  pub(crate) class: Identifier,
  pub(crate) identifier: Identifier,
  pub(crate) params: Vec<SubroutineDeclarationParam>,
  pub(crate) return_type: TypeDeclaration,
  pub(crate) body: Block,
}

impl FromTokenizer for SubroutineDeclaration {
  fn from_tokenizer(
    file_info: &FileInfo,
    scope_gen: &mut ScopeGenerator,
    tokenizer: &mut Tokenizer,
    pos: usize,
  ) -> Self {
    let class = scope_gen
      .get_class()
      .expect(error_fmt_src!(
        "Tokenizer could not provide the current class name."
      ))
      .clone();

    let function_type = tokenizer.next().expect(error_fmt_file!(
      &file_info.name,
      &file_info.content,
      pos,
      "(ParseError)",
      "Expected (`{}`, `{}` or `{}`). Found: (EOF).",
      Keyword::Constructor,
      Keyword::Function,
      Keyword::Method
    ));

    let Token::Keyword(function_type, _) = function_type else {
      error_panic_fmt_file!(
        &file_info.name,
        &file_info.content,
        pos,
        "(ParseError)",
        "Expected (keyword token: `constructor`, `function` \
          or `method`). Found: ({}).",
        function_type
      )
    };

    let mut params = Vec::new();

    let return_type_token = tokenizer.next().expect(error_fmt_file!(
      &file_info.name,
      &file_info.content,
      pos,
      "(ParseError)",
      "Expected (`return type declaration`). Found: (EOF)."
    ));

    let return_type = TypeDeclaration {
      pos,
      raw_type: Type::parse(&return_type_token).expect(error_fmt_file!(
        &file_info.name,
        &file_info.content,
        pos,
        "(ParseError)",
        "Could not parse return type `{}`.",
        return_type_token
      )),
    };

    let identifier =
      Identifier::from_tokenizer(file_info, scope_gen, tokenizer, pos);

    scope_gen.set_function(&function_type, &identifier);
    let scope = scope_gen.create_scope_info();

    tokenizer.expect(Token::Symbol(Symbol::LeftParen, 0));

    while let Some(token) = tokenizer.next() {
      match token {
        Token::Symbol(Symbol::RightParen, _) => break,
        Token::Identifier(_, pos) | Token::Keyword(_, pos) => {
          params.push(SubroutineDeclarationParam {
            identifier: Identifier::from_tokenizer(
              file_info, scope_gen, tokenizer, pos,
            ),
            type_dec: TypeDeclaration {
              pos,
              raw_type: Type::parse(&token).expect(error_fmt_file!(
                &file_info.name,
                &file_info.content,
                pos,
                "(ParseError)",
                "Invalid type declaration `{}`.",
                token
              )),
            },
          });
        }
        Token::Symbol(Symbol::Comma, _) => (),
        _ => error_panic_fmt_file!(
          &file_info.name,
          &file_info.content,
          token.get_pos(),
          "(ParseError)",
          "Unexpected token in function declaration. Expected \
            (`type declaration`, `{}` or `{}`). Found: ({}).",
          Symbol::Comma,
          Symbol::RightParen,
          token
        ),
      }
    }

    let body = Block::from_tokenizer(file_info, scope_gen, tokenizer, pos);

    return Self {
      pos,
      scope,
      subroutine_type: function_type,
      class,
      identifier,
      params,
      return_type,
      body,
    };
  }
}

#[derive(Debug, Clone)]
pub(crate) struct SubroutineDeclarationParam {
  pub(crate) identifier: Identifier,
  pub(crate) type_dec: TypeDeclaration,
}

#[derive(Clone, Debug)]
pub(crate) struct TypeDeclaration {
  pub(crate) pos: usize,
  pub(crate) raw_type: Type,
}

////////////////////////////////////////////////////////////////////////////////
// Subroutine Calls & Assignments
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub(crate) struct SubroutineCall {
  pub(crate) pos: usize,
  pub(crate) scope: ScopeInfo,
  pub(crate) has_class_specifier: bool,
  pub(crate) has_this_class_specifier: bool,
  pub(crate) class_var_ref: Identifier,
  pub(crate) call_type: Option<Keyword>,
  pub(crate) identifier: Identifier,
  pub(crate) params: Vec<ExpressionInfo>,
}

impl FromTokenizer for SubroutineCall {
  fn from_tokenizer(
    file_info: &FileInfo,
    scope_gen: &mut ScopeGenerator,
    tokenizer: &mut Tokenizer,
    pos: usize,
  ) -> Self {
    let scope = scope_gen.create_scope_info();
    let class_var_ref;
    let mut has_class_specifier = false;
    let mut has_this_class_specifier = false;

    let call_type = match tokenizer.peek(0) {
      Some(Token::Keyword(kw, _)) => {
        tokenizer.next();
        Some(kw)
      }
      _ => None,
    };

    if let Some(Token::Keyword(kw, _)) = tokenizer.peek(0) {
      if kw == Keyword::This {
        if let Some(Keyword::Function) = scope_gen.get_subroutine_type() {
          error_panic_fmt_file!(
            &file_info.name,
            &file_info.content,
            tokenizer.next().unwrap().get_pos(),
            "(ParseError)",
            "Found keyword `{}` in {} scope `{}`. Cannot reference \
              object instance in static functions.",
            Keyword::This,
            Keyword::Function,
            scope
          );
        }
        has_this_class_specifier = true;
        tokenizer.next();
        tokenizer.expect(Token::Symbol(Symbol::Dot, 0));
      }
    }

    let mut identifier =
      Identifier::from_tokenizer(file_info, scope_gen, tokenizer, pos);

    if let Some(Token::Symbol(Symbol::Dot, _)) = tokenizer.peek(0) {
      has_class_specifier = true;
      tokenizer.next();
      class_var_ref = identifier;
      identifier =
        Identifier::from_tokenizer(file_info, scope_gen, tokenizer, pos);
    } else {
      class_var_ref = scope_gen
        .get_class()
        .expect(error_fmt_src!(
          "Tokenizer could not provide the current class name."
        ))
        .clone();
    }

    let mut params = Vec::new();

    tokenizer.expect(Token::Symbol(Symbol::LeftParen, 0));

    while let Some(token) = tokenizer.peek(0) {
      match token {
        Token::Symbol(Symbol::RightParen, _) => {
          tokenizer.next();
          break;
        }
        Token::Symbol(Symbol::Comma, _) => {
          tokenizer.next();
        }
        _ => {
          params.push(Ast::parse_expression(file_info, scope_gen, tokenizer));
        }
      }
    }

    return Self {
      pos,
      scope,
      has_class_specifier,
      has_this_class_specifier,
      class_var_ref,
      call_type,
      identifier,
      params,
    };
  }
}

#[derive(Debug, Clone)]
pub(crate) struct VariableAssignment {
  pub(crate) pos: usize,
  pub(crate) scope: ScopeInfo,
  pub(crate) is_class_var: bool,
  pub(crate) identifier: Identifier,
  pub(crate) value: ExpressionInfo,
}

impl FromTokenizer for VariableAssignment {
  fn from_tokenizer(
    file_info: &FileInfo,
    scope_gen: &mut ScopeGenerator,
    tokenizer: &mut Tokenizer,
    pos: usize,
  ) -> Self {
    let scope = scope_gen.create_scope_info();
    let mut is_class_var = false;

    tokenizer.expect(Token::Keyword(Keyword::Let, 0));

    if tokenizer.peek(0) == Some(Token::Keyword(Keyword::This, 0)) {
      if let Some(Keyword::Function) = scope_gen.get_subroutine_type() {
        error_panic_fmt_file!(
          &file_info.name,
          &file_info.content,
          tokenizer.next().unwrap().get_pos(),
          "(ParseError)",
          "Found keyword `{}` in {} scope `{}`. Cannot reference \
            object instance in static functions.",
          Keyword::This,
          Keyword::Function,
          scope
        );
      }

      tokenizer.expect(Token::Keyword(Keyword::This, 0));
      tokenizer.expect(Token::Symbol(Symbol::Dot, 0));
      is_class_var = true;
    }

    let identifier =
      Identifier::from_tokenizer(file_info, scope_gen, tokenizer, pos);

    tokenizer.expect(Token::Symbol(Symbol::Eq, 0));

    let value = Ast::parse_expression(file_info, scope_gen, tokenizer);

    tokenizer.expect(Token::Symbol(Symbol::Semicolon, 0));

    return Self {
      pos,
      scope,
      is_class_var,
      identifier,
      value,
    };
  }
}

////////////////////////////////////////////////////////////////////////////////
// Expression Parsing
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub(crate) struct ExpressionInfo {
  pub(crate) expr_type: RefCell<Type>,
  pub(crate) expr: Expression,
  pub(crate) bracketed: bool,
}

impl ExpressionInfo {
  pub(crate) fn new(expr: Expression) -> Self {
    return Self {
      expr_type: RefCell::new(Type::Void),
      expr,
      bracketed: false,
    };
  }

  pub(crate) fn set_brackets(mut self, value: bool) -> Self {
    self.bracketed = value;
    return self;
  }
}

#[derive(Debug, Clone)]
pub(crate) enum Expression {
  Primary(Primary),
  Unary {
    op: (Operator, usize),
    rhs: Box<ExpressionInfo>,
  },
  BinOp {
    lhs: Box<ExpressionInfo>,
    op: (Operator, usize),
    rhs: Box<ExpressionInfo>,
  },
}

#[derive(Debug, Clone)]
pub(crate) enum Primary {
  Integer(Integer),
  Character(Character),
  SizedString(SizedString),
  Boolean(Boolean),
  Null(Null),
  This(This),
  Identifier(Identifier),
  SubroutineCall(SubroutineCall),
}

struct ExpressionBuilder<'a> {
  file_info: &'a FileInfo,
  unary: Option<(Operator, usize)>,
  lhs: Option<ExpressionInfo>,
  op: Option<(Operator, usize)>,
  rhs: Option<ExpressionInfo>,
  bracketed: bool,
}

impl<'a> ExpressionBuilder<'a> {
  fn new(file_info: &'a FileInfo) -> Self {
    return Self {
      file_info,
      unary: None,
      lhs: None,
      op: None,
      rhs: None,
      bracketed: false,
    };
  }

  fn add_expr(&mut self, expr: ExpressionInfo, token: &Token) {
    if self.lhs.is_none() {
      self.lhs = Some(expr);
      return;
    } else if self.lhs.is_some()
      && self.op.is_some()
      && self.unary.is_some()
      && self.rhs.is_none()
    {
      self.rhs = Some(ExpressionInfo::new(Expression::Unary {
        op: self.unary.take().unwrap(),
        rhs: Box::new(expr),
      }));
      return;
    } else if self.op.is_some() && self.rhs.is_none() {
      self.rhs = Some(expr);
      return;
    }

    error_panic_fmt_file!(
      &self.file_info.name,
      &self.file_info.content,
      token.get_pos(),
      "(ParseError)",
      "Unexpected token while parsing expression. Expected \
        (´expression´). Found: ({}).",
      token
    )
  }

  fn add_op(&mut self, token: Token) {
    if self.lhs.is_some() && self.op.is_some() && self.rhs.is_some() {
      self.lhs = Some(
        ExpressionInfo::new(Expression::BinOp {
          lhs: Box::new(self.lhs.take().unwrap()),
          op: self.op.take().unwrap(),
          rhs: Box::new(self.rhs.take().unwrap()),
        })
        .set_brackets(self.bracketed),
      );

      self.bracketed = false;
    } else if self.lhs.is_none() && self.unary.is_some() {
      let (current_unary, pos) = self.unary.take().unwrap();

      match current_unary {
        Operator::Add => {
          if token == Token::Symbol(Symbol::Plus, pos) {
            self.unary = None;
          } else if token == Token::Symbol(Symbol::Minus, pos) {
            self.unary = Some((Operator::Sub, pos));
          } else {
            error_panic_fmt_file!(
              &self.file_info.name,
              &self.file_info.content,
              token.get_pos(),
              "(ParseError)",
              "Unexpected token in numeric unary expression. Expected \
                (`{}` or `{}`). Found: ({}).",
              Symbol::Plus,
              Symbol::Minus,
              token
            )
          }
        }
        Operator::Sub => {
          if token == Token::Symbol(Symbol::Plus, pos) {
            self.unary = Some((Operator::Sub, pos));
          } else if token == Token::Symbol(Symbol::Minus, pos) {
            self.unary = None;
          } else {
            error_panic_fmt_file!(
              &self.file_info.name,
              &self.file_info.content,
              token.get_pos(),
              "(ParseError)",
              "Unexpected token in numeric unary expression. Expected \
                (`{}` or `{}`). Found: ({}).",
              Symbol::Plus,
              Symbol::Minus,
              token
            )
          }
        }
        Operator::Not => {
          if token == Token::Symbol(Symbol::Tilde, pos) {
            self.unary = None;
          } else {
            error_panic_fmt_file!(
              &self.file_info.name,
              &self.file_info.content,
              token.get_pos(),
              "(ParseError)",
              "Unexpected token in boolean unary expression. Expected \
                (`{}`). Found: ({}).",
              Symbol::Tilde,
              token
            )
          }
        }
        _ => error_panic_fmt_file!(
          &self.file_info.name,
          &self.file_info.content,
          token.get_pos(),
          "(ParseError)",
          "Unexpected token in unary expression. Expected (`{}, {} or {}`). \
            Found: ({}).",
          Symbol::Tilde,
          Symbol::Plus,
          Symbol::Minus,
          token
        ),
      }

      if self.unary.is_none() {
        warning_println_file!(
          &self.file_info.name,
          &self.file_info.content,
          pos,
          "(TypeChecker)",
          "Unnecessary unary operator canceled out by subsequent unary \
            operator `{}`.",
          current_unary
        );
      }
    } else if (self.lhs.is_some()
      && self.op.is_some()
      && self.unary.is_none()
      && self.rhs.is_none())
      || (self.lhs.is_none() && self.unary.is_none())
    {
      let (op, pos): (Operator, usize) = token.into();

      if op.is_valid_unary_op() {
        if op == Operator::Add {
          warning_println_file!(
            &self.file_info.name,
            &self.file_info.content,
            pos,
            "(TypeChecker)",
            "Redundant unary operator `{}` will be ignored when evaluating \
              numeric unary expressions as it has no effect.",
            Operator::Add
          );
        }
        self.unary = Some((op, pos));
      }

      return;
    } else if self.lhs.is_some() && self.unary.is_some() {
      self.lhs = Some(
        ExpressionInfo::new(Expression::Unary {
          op: self.unary.take().unwrap(),
          rhs: Box::new(self.lhs.take().unwrap()),
        })
        .set_brackets(self.bracketed),
      );

      self.bracketed = false;
    }

    self.op = Some(token.into());
  }

  fn build(&mut self) -> ExpressionInfo {
    if self.lhs.is_some() && self.op.is_some() && self.rhs.is_some() {
      return ExpressionInfo::new(Expression::BinOp {
        lhs: Box::new(self.lhs.take().unwrap()),
        op: self.op.take().unwrap(),
        rhs: Box::new(self.rhs.take().unwrap()),
      });
    } else if self.lhs.is_some() && self.unary.is_some() {
      return ExpressionInfo::new(Expression::Unary {
        op: self.unary.take().unwrap(),
        rhs: Box::new(self.lhs.take().unwrap()),
      });
    } else if self.lhs.is_some() {
      return self.lhs.take().unwrap();
    } else {
      error_unreachable!(
        "Trying to build expression from ExpressionBuilder \
          when it is not possible.\nCurrent expression:\
          \nLHS: ({:#?})\nOP: ({:#?})\nRHS: ({:#?}).",
        self.lhs,
        self.op,
        self.rhs
      );
    }
  }
}

////////////////////////////////////////////////////////////////////////////////
// Program
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub(crate) struct Program {
  pub(crate) classes: Vec<ClassDeclaration>,
  pub(crate) tokens: Vec<Token>,
  pub(crate) instructions: Vec<JackToken>,
}

impl Program {
  pub(crate) fn new(
    classes: Vec<ClassDeclaration>,
    tokens: Vec<Token>,
  ) -> Self {
    return Self {
      classes,
      tokens,
      instructions: Vec::new(),
    };
  }
}

////////////////////////////////////////////////////////////////////////////////
// AST Implementation
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
pub(crate) struct Ast {
  pub(crate) program: Program,
}

impl Ast {
  fn new(program: Program) -> Self {
    return Self { program };
  }

  pub(crate) fn parse(file_info: &FileInfo) -> Result<Self> {
    let mut tokenizer = Tokenizer::new(file_info);
    let mut scope_gen = ScopeGenerator::default();
    let mut classes = Vec::new();

    while let Some(token) = tokenizer.peek(0) {
      match token {
        Token::Keyword(Keyword::Class, pos) => {
          classes.push(ClassDeclaration::from_tokenizer(
            file_info,
            &mut scope_gen,
            &mut tokenizer,
            pos,
          ))
        }
        _ => error_panic_fmt_file!(
          &file_info.name,
          &file_info.content,
          token.get_pos(),
          "(ParseError)",
          "Unexpected token in program body. Expected \
            (`class declaration`). Found: ({}).",
          token
        ),
      }
    }

    return Ok(Self::new(Program::new(
      classes,
      tokenizer.get_tokens().to_vec(),
    )));
  }

  fn parse_expression(
    file_info: &FileInfo,
    scope_gen: &mut ScopeGenerator,
    tokenizer: &mut Tokenizer,
  ) -> ExpressionInfo {
    let mut builder = ExpressionBuilder::new(file_info);
    let scope = scope_gen.create_scope_info();

    while let Some(token) = tokenizer.peek(0) {
      match token {
        Token::IntegerConstant(_, pos) => builder.add_expr(
          ExpressionInfo::new(Expression::Primary(Primary::Integer(
            Integer::from_tokenizer(file_info, scope_gen, tokenizer, pos),
          ))),
          &token,
        ),
        Token::CharacterConstant(_, pos) => builder.add_expr(
          ExpressionInfo::new(Expression::Primary(Primary::Character(
            Character::from_tokenizer(file_info, scope_gen, tokenizer, pos),
          ))),
          &token,
        ),
        Token::StringConstant(_, pos) => builder.add_expr(
          ExpressionInfo::new(Expression::Primary(Primary::SizedString(
            SizedString::from_tokenizer(file_info, scope_gen, tokenizer, pos),
          ))),
          &token,
        ),
        Token::Keyword(ref kw, pos) => match *kw {
          Keyword::False | Keyword::True => {
            builder.add_expr(
              ExpressionInfo::new(Expression::Primary(Primary::Boolean(
                Boolean::from_tokenizer(file_info, scope_gen, tokenizer, pos),
              ))),
              &token,
            );
          }
          Keyword::Null => {
            builder.add_expr(
              ExpressionInfo::new(Expression::Primary(Primary::Null(
                Null::from_tokenizer(file_info, scope_gen, tokenizer, pos),
              ))),
              &token,
            );
          }
          Keyword::This => {
            let next = tokenizer.peek(1);

            if let Some(Keyword::Function) = scope_gen.get_subroutine_type() {
              error_panic_fmt_file!(
                &file_info.name,
                &file_info.content,
                token.get_pos(),
                "(ParseError)",
                "Found keyword `{}` in {} scope `{}`. Cannot reference \
                  object instance in static functions.",
                Keyword::This,
                Keyword::Function,
                scope
              )
            }

            match next {
              Some(Token::Symbol(Symbol::Dot, _)) => {
                tokenizer.expect(Token::Keyword(Keyword::This, 0));
                tokenizer.expect(Token::Symbol(Symbol::Dot, 0));

                let next = tokenizer.peek(1);

                match next {
                  Some(Token::Symbol(Symbol::Dot, _))
                  | Some(Token::Symbol(Symbol::LeftParen, _)) => {
                    builder.add_expr(
                      ExpressionInfo::new(Expression::Primary(
                        Primary::SubroutineCall(
                          SubroutineCall::from_tokenizer(
                            file_info, scope_gen, tokenizer, pos,
                          ),
                        ),
                      )),
                      &token,
                    );
                  }
                  _ => {
                    let mut ident = Identifier::from_tokenizer(
                      file_info, scope_gen, tokenizer, pos,
                    );
                    ident.is_class_var = true;

                    builder.add_expr(
                      ExpressionInfo::new(Expression::Primary(
                        Primary::Identifier(ident),
                      )),
                      &token,
                    );
                  }
                }
              }
              _ => {
                builder.add_expr(
                  ExpressionInfo::new(Expression::Primary(Primary::This(
                    This::from_tokenizer(file_info, scope_gen, tokenizer, pos),
                  ))),
                  &token,
                );
              }
            }
          }
          _ => {
            tokenizer.next();
            break;
          }
        },
        Token::Identifier(_, pos) => {
          let next = tokenizer.peek(1);

          match next {
            Some(Token::Symbol(Symbol::Dot, _))
            | Some(Token::Symbol(Symbol::LeftParen, _)) => {
              builder.add_expr(
                ExpressionInfo::new(Expression::Primary(
                  Primary::SubroutineCall(SubroutineCall::from_tokenizer(
                    file_info, scope_gen, tokenizer, pos,
                  )),
                )),
                &token,
              );
            }
            _ => {
              builder.add_expr(
                ExpressionInfo::new(Expression::Primary(Primary::Identifier(
                  Identifier::from_tokenizer(
                    file_info, scope_gen, tokenizer, pos,
                  ),
                ))),
                &token,
              );
            }
          }
        }
        Token::Symbol(ref symbol, pos) => {
          if symbol.is_operator() {
            builder.add_op(tokenizer.next().unwrap());
            continue;
          } else {
            match symbol {
              Symbol::Comma
              | Symbol::Semicolon
              | Symbol::RightCurlyBrace
              | Symbol::RightSquareBracket => {
                break;
              }
              Symbol::LeftParen => {
                tokenizer.next();
                builder.add_expr(
                  Ast::parse_expression(file_info, scope_gen, tokenizer)
                    .set_brackets(true),
                  &token,
                );

                tokenizer.expect(Token::Symbol(Symbol::RightParen, 0));
              }
              Symbol::RightParen => {
                break;
              }
              _ => {
                error_panic_fmt_file!(
                  &file_info.name,
                  &file_info.content,
                  pos,
                  "(ParseError)",
                  "Unexpected token while parsing expression. Expected \
                    (`operator`). Found: ({}).",
                  token
                )
              }
            }
          }
        }
      }
    }

    return builder.build();
  }
}
