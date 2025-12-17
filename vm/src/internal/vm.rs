////////////////////////////////////////////////////////////////////////////////
// File: src/internal/vm.rs
// Description: Parser module
//
// Author: Leon Heidelbach <leon.heidelbach@hhu.de>
// Date: 06.05.2024
//
// License: GPLv3
////////////////////////////////////////////////////////////////////////////////

use core::fmt;
use std::fmt::{Display, Formatter};

////////////////////////////////////////////////////////////////////////////////
// Constants
////////////////////////////////////////////////////////////////////////////////

const ARG_SEGMENT: &str = "ARG";
const CONST_SEGMENT: &str = "CONST";
const LOCAL_SEGMENT: &str = "LCL";
const TEMP_SEGMENT: &str = "TMP";
const THAT_SEGMENT: &str = "THAT";
const THIS_SEGMENT: &str = "THIS";
const STATIC_SEGMENT: &str = "STATIC";
const PTR_SEGMENT: &str = "PTR";

////////////////////////////////////////////////////////////////////////////////
// Segment implementation
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub enum Segment {
  Argument,
  Constant,
  Local,
  Temp,
  That,
  This,
  Static,
  Pointer,
  FunctionName(String),
}

impl Segment {
  pub(crate) fn as_str(&self) -> &str {
    match self {
      Segment::Argument => return ARG_SEGMENT,
      Segment::Constant => return CONST_SEGMENT,
      Segment::Local => return LOCAL_SEGMENT,
      Segment::Temp => return TEMP_SEGMENT,
      Segment::That => return THAT_SEGMENT,
      Segment::This => return THIS_SEGMENT,
      Segment::Static => return STATIC_SEGMENT,
      Segment::Pointer => return PTR_SEGMENT,
      Segment::FunctionName(name) => return name,
    }
  }
}

impl Display for Segment {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      Segment::Argument => return write!(f, "{}", ARG_SEGMENT),
      Segment::Constant => return write!(f, "{}", CONST_SEGMENT),
      Segment::Local => return write!(f, "{}", LOCAL_SEGMENT),
      Segment::Temp => return write!(f, "{}", TEMP_SEGMENT),
      Segment::That => return write!(f, "{}", THAT_SEGMENT),
      Segment::This => return write!(f, "{}", THIS_SEGMENT),
      Segment::Static => return write!(f, "{}", STATIC_SEGMENT),
      Segment::Pointer => return write!(f, "{}", PTR_SEGMENT),
      Segment::FunctionName(name) => return write!(f, "{}", name),
    }
  }
}

////////////////////////////////////////////////////////////////////////////////
// VM Instruction Tokens
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub enum Token {
  Push(Push),
  Pop(Pop),
  Goto(Goto),
  IfGoto(IfGoto),
  Label(Label),
  FunctionDecl(FunctionDecl),
  FunctionCall(FunctionCall),
  Add(Add),
  Sub(Sub),
  Neg(Neg),
  Eq(Eq),
  Gt(Gt),
  Lt(Lt),
  And(And),
  Or(Or),
  Not(Not),
  Return(Return),
}

////////////////////////////////////////////////////////////////////////////////
// Token Tuple Structs
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub(crate) struct Push(pub(crate) Segment, pub(crate) u16);

#[derive(Debug, Clone)]
pub(crate) struct Pop(pub(crate) Segment, pub(crate) u16);

#[derive(Debug, Clone)]
pub(crate) struct Goto(pub(crate) String);

#[derive(Debug, Clone)]
pub(crate) struct IfGoto(pub(crate) String);

#[derive(Debug, Clone)]
pub(crate) struct Label(pub(crate) String);

#[derive(Debug, Clone)]
pub(crate) struct FunctionDecl(pub(crate) Segment, pub(crate) u16);

#[derive(Debug, Clone)]
pub(crate) struct FunctionCall(pub(crate) Segment, pub(crate) u16);

////////////////////////////////////////////////////////////////////////////////
// Token Unit-Like Structs
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub(crate) struct Add;

#[derive(Debug, Clone)]
pub(crate) struct Sub;

#[derive(Debug, Clone)]
pub(crate) struct Neg;

#[derive(Debug, Clone)]
pub(crate) struct Eq;

#[derive(Debug, Clone)]
pub(crate) struct Gt;

#[derive(Debug, Clone)]
pub(crate) struct Lt;

#[derive(Debug, Clone)]
pub(crate) struct And;

#[derive(Debug, Clone)]
pub(crate) struct Or;

#[derive(Debug, Clone)]
pub(crate) struct Not;

#[derive(Debug, Clone)]
pub(crate) struct Return;
