////////////////////////////////////////////////////////////////////////////////
// File: src/internal/jack.rs
// Description: Jack module
//
// Author: Leon Heidelbach <leon.heidelbach@hhu.de>
// Date: 15.05.2024
//
// License: GPLv3
////////////////////////////////////////////////////////////////////////////////

use core::fmt;
use std::fmt::{Display, Formatter};

use super::{ast::Operator, tokenize::Keyword};

////////////////////////////////////////////////////////////////////////////////
// Jack Tokens
////////////////////////////////////////////////////////////////////////////////

#[allow(non_camel_case_types)]
#[derive(Debug, Clone)]
pub(crate) enum JackToken {
  call(String, u16),
  function(String, u16),
  goto(String),
  if_goto(String),
  label(String),
  pop(VMSegment, u16),
  push(VMSegment, u16),
  comment(String),
  r#return,
  add,
  and,
  eq,
  gt,
  lt,
  neg,
  not,
  or,
  sub,
}

impl From<Operator> for JackToken {
  fn from(op: Operator) -> Self {
    match op {
      Operator::Add => return JackToken::add,
      Operator::And => return JackToken::and,
      Operator::Eq => return JackToken::eq,
      Operator::Gt => return JackToken::gt,
      Operator::Lt => return JackToken::lt,
      Operator::Not => return JackToken::neg,
      Operator::Or => return JackToken::or,
      Operator::Sub => return JackToken::sub,
      Operator::Mul => {
        return JackToken::call(String::from("Math.multiply"), 2)
      }
      Operator::Div => return JackToken::call(String::from("Math.divide"), 2),
    }
  }
}

impl Display for JackToken {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      JackToken::call(name, n_args) => {
        return write!(f, "call {} {}", name, n_args)
      }
      JackToken::function(name, n_locals) => {
        return write!(f, "function {} {}", name, n_locals)
      }
      JackToken::goto(label) => return write!(f, "goto {}", label),
      JackToken::if_goto(label) => return write!(f, "if-goto {}", label),
      JackToken::label(label) => return write!(f, "label {}", label),
      JackToken::pop(segment, index) => {
        return write!(f, "pop {} {}", segment, index)
      }
      JackToken::push(segment, index) => {
        return write!(f, "push {} {}", segment, index)
      }
      JackToken::comment(comment) => return write!(f, "// {}", comment),
      JackToken::r#return => return write!(f, "return"),
      JackToken::add => return write!(f, "add"),
      JackToken::and => return write!(f, "and"),
      JackToken::eq => return write!(f, "eq"),
      JackToken::gt => return write!(f, "gt"),
      JackToken::lt => return write!(f, "lt"),
      JackToken::neg => return write!(f, "neg"),
      JackToken::not => return write!(f, "not"),
      JackToken::or => return write!(f, "or"),
      JackToken::sub => return write!(f, "sub"),
    }
  }
}

////////////////////////////////////////////////////////////////////////////////
// Segment Types & Associated Keywords
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum SymbolKind {
  Static,
  Field,
  Argument,
  Local,
}

impl Display for SymbolKind {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      SymbolKind::Static => return write!(f, "static"),
      SymbolKind::Field => return write!(f, "field"),
      SymbolKind::Argument => return write!(f, "argument"),
      SymbolKind::Local => return write!(f, "local"),
    }
  }
}

impl From<Keyword> for SymbolKind {
  fn from(keyword: Keyword) -> Self {
    match keyword {
      Keyword::Static => return SymbolKind::Static,
      Keyword::Field => return SymbolKind::Field,
      Keyword::Var => return SymbolKind::Local,
      _ => return SymbolKind::Argument,
    }
  }
}

#[derive(Debug, Clone)]
pub(crate) enum VMSegment {
  Argument,
  Constant,
  Local,
  Pointer,
  Static,
  Temp,
  That,
  This,
}

impl Display for VMSegment {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      VMSegment::Argument => return write!(f, "argument"),
      VMSegment::Constant => return write!(f, "constant"),
      VMSegment::Local => return write!(f, "local"),
      VMSegment::Pointer => return write!(f, "pointer"),
      VMSegment::Static => return write!(f, "static"),
      VMSegment::Temp => return write!(f, "temp"),
      VMSegment::That => return write!(f, "that"),
      VMSegment::This => return write!(f, "this"),
    }
  }
}
