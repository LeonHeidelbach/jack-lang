////////////////////////////////////////////////////////////////////////////////
// File: src/internal/asm.rs
// Description: Assembler token module
//
// Author: Leon Heidelbach <leon.heidelbach@hhu.de>
// Date: 01.05.2024
//
// License: GPLv3
////////////////////////////////////////////////////////////////////////////////

use std::{cell::Cell, rc::Rc};

////////////////////////////////////////////////////////////////////////////////
// Token Types
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
pub(crate) enum Token {
  A(AToken),
  C(CToken),
  Label,
}

#[derive(Debug)]
pub(crate) enum AToken {
  Literal(u16),
  Mnemonic(Rc<Cell<u16>>),
}

#[derive(Debug)]
pub(crate) struct CDestinationToken {
  pub(crate) a_reg: bool,
  pub(crate) d_reg: bool,
  pub(crate) ram: bool,
}

#[derive(Debug)]
pub(crate) struct CJumpToken {
  pub(crate) eq: bool,
  pub(crate) gt: bool,
  pub(crate) lt: bool,
}

#[allow(clippy::upper_case_acronyms)]
#[derive(Debug)]
pub(crate) enum CCompToken {
  ARegister,
  ARegisterMinusDRegister,
  ARegisterMinusOne,
  ARegisterPlusOne,
  DRegister,
  DRegisterAndARegister,
  DRegisterAndRAM,
  DRegisterMinusARegister,
  DRegisterMinusOne,
  DRegisterMinusRAM,
  DRegisterOrARegister,
  DRegisterOrRAM,
  DRegisterPlusARegister,
  DRegisterPlusOne,
  DRegisterPlusRAM,
  MinusARegister,
  MinusDRegister,
  MinusOne,
  MinusRAM,
  NotARegister,
  NotDRegister,
  NotRAM,
  One,
  RAMMinusDRegister,
  RAMMinusOne,
  RAMPlusOne,
  RAM,
  Zero,
}

#[derive(Debug)]
pub(crate) struct CToken {
  pub(crate) cmp: CCompToken,
  pub(crate) dst: CDestinationToken,
  pub(crate) jmp: CJumpToken,
}
