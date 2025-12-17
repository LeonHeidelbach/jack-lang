////////////////////////////////////////////////////////////////////////////////
// File: src/internal/parse.rs
// Description: Parser module
//
// Author: Leon Heidelbach <leon.heidelbach@hhu.de>
// Date: 01.05.2024
//
// License: GPLv3
////////////////////////////////////////////////////////////////////////////////

use std::{cell::Cell, collections::HashMap, rc::Rc, str::Lines};

use crate::internal::asm::AToken;

use super::{
  asm::{CCompToken, CDestinationToken, CJumpToken, CToken, Token},
  intrinsics::INTRINSICS,
};

use anyhow::Result;
use indexmap::IndexMap;
use shared::{
  error_fmt_src, error_panic_fmt_file, error_panic_src, info_print,
  io::os::FileInfo,
  util::{
    helpers::{FindIgnoringWhitespace, StrManipulation, U16Manipulation},
    settings::Setting,
  },
};

////////////////////////////////////////////////////////////////////////////////
// Program
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
pub(crate) struct Program {
  pub(crate) tokens: Vec<Token>,
}

////////////////////////////////////////////////////////////////////////////////
// Assembly Parser
////////////////////////////////////////////////////////////////////////////////

pub(crate) struct AsmParser<'a> {
  pos: usize,
  address: u16,
  ptr: u16,
  file_info: &'a FileInfo,
  file_lines: Lines<'a>,
  symbol_table: HashMap<String, Rc<Cell<u16>>>,
  address_order: IndexMap<String, Rc<Cell<u16>>>,
}

impl<'a> AsmParser<'a> {
  pub(crate) fn parse(file_info: &'a FileInfo) -> Result<Program> {
    info_print!("Assembling file: `{}`", file_info.name);

    let parser = Self {
      pos: 0,
      address: 16,
      ptr: 0,
      file_info,
      file_lines: file_info.content.lines(),
      symbol_table: HashMap::new(),
      address_order: IndexMap::new(),
    };

    let program = Program {
      tokens: parser.collect(),
    };

    if Setting::PrintAll.is_set().is_ok()
      || Setting::PrintProgram.is_set().is_ok()
    {
      info_print!("Program");
      println!("{:#?}", program);
    }

    return Ok(program);
  }

  pub(crate) fn assign_addresses(&mut self) {
    for (_, addr_ref) in self.address_order.iter() {
      addr_ref.replace(self.address.get_value_and_increment());
    }
  }

  fn match_comp_token(
    &self,
    token: Option<&str>,
    raw_line: &str,
    raw_line_len: usize,
  ) -> CCompToken {
    if let Some(token) = token {
      return match token {
        "0" => CCompToken::Zero,
        "1" => CCompToken::One,
        "-1" => CCompToken::MinusOne,
        "D" => CCompToken::DRegister,
        "A" => CCompToken::ARegister,
        "M" => CCompToken::RAM,
        "!D" => CCompToken::NotDRegister,
        "!A" => CCompToken::NotARegister,
        "!M" => CCompToken::NotRAM,
        "-D" => CCompToken::MinusDRegister,
        "-A" => CCompToken::MinusARegister,
        "-M" => CCompToken::MinusRAM,
        "D+1" => CCompToken::DRegisterPlusOne,
        "A+1" => CCompToken::ARegisterPlusOne,
        "M+1" => CCompToken::RAMPlusOne,
        "D-1" => CCompToken::DRegisterMinusOne,
        "A-1" => CCompToken::ARegisterMinusOne,
        "M-1" => CCompToken::RAMMinusOne,
        "D+A" => CCompToken::DRegisterPlusARegister,
        "D+M" => CCompToken::DRegisterPlusRAM,
        "D-A" => CCompToken::DRegisterMinusARegister,
        "D-M" => CCompToken::DRegisterMinusRAM,
        "A-D" => CCompToken::ARegisterMinusDRegister,
        "M-D" => CCompToken::RAMMinusDRegister,
        "D&A" => CCompToken::DRegisterAndARegister,
        "D&M" => CCompToken::DRegisterAndRAM,
        "D|A" => CCompToken::DRegisterOrARegister,
        "D|M" => CCompToken::DRegisterOrRAM,
        _ => error_panic_fmt_file!(
          &self.file_info.name,
          &self.file_info.content,
          self.pos
            - (raw_line_len
              - raw_line.find_ignoring_whitespaces(token).unwrap_or(0)),
          "(ParseError)",
          "Unknown instruction: `{}`",
          token
        ),
      };
    } else {
      error_panic_src!("No comparison token found");
    };
  }

  fn match_jump_token(
    &self,
    token: Option<&str>,
    raw_line: &str,
    raw_line_len: usize,
  ) -> CJumpToken {
    if let Some(token) = token {
      return match token {
        "JGT" => CJumpToken {
          eq: false,
          gt: true,
          lt: false,
        },
        "JEQ" => CJumpToken {
          eq: true,
          gt: false,
          lt: false,
        },
        "JGE" => CJumpToken {
          eq: true,
          gt: true,
          lt: false,
        },
        "JLT" => CJumpToken {
          eq: false,
          gt: false,
          lt: true,
        },
        "JNE" => CJumpToken {
          eq: false,
          gt: true,
          lt: true,
        },
        "JLE" => CJumpToken {
          eq: true,
          gt: false,
          lt: true,
        },
        "JMP" => CJumpToken {
          eq: true,
          gt: true,
          lt: true,
        },
        _ => {
          error_panic_fmt_file!(
            &self.file_info.name,
            &self.file_info.content,
            self.pos
              - (raw_line_len
                - raw_line.find_ignoring_whitespaces(token).unwrap_or(0)),
            "(ParseError)",
            "Unknown jump instruction: `{}`",
            token
          );
        }
      };
    } else {
      error_panic_src!("No jump token found");
    };
  }

  fn parse_label(
    &mut self,
    line: &str,
    raw_line: &str,
    raw_line_len: usize,
  ) -> Option<Token> {
    if line.starts_with('(') {
      let label = line.trim_start_matches('(').trim_end_matches(')');

      if INTRINSICS.contains_key(label) {
        error_panic_fmt_file!(
          &self.file_info.name,
          &self.file_info.content,
          self.pos
            - (raw_line_len
              - raw_line.find_ignoring_whitespaces(label).unwrap_or(0)),
          "(ParseError)",
          "Intrinsics cannot be redefined: {}",
          label
        );
      }

      if let Some(address) = self.symbol_table.get(label) {
        self.address_order.shift_remove(label);

        if address.get() != self.ptr {
          address.replace(self.ptr);

          return Some(Token::Label);
        }
      } else {
        let address = Rc::new(Cell::new(self.ptr));

        self.symbol_table.insert(label.to_string(), address.clone());

        return Some(Token::Label);
      }
    }

    return None;
  }

  fn parse_a_token(&mut self, line: &str) -> Option<Token> {
    if line.starts_with('@') {
      if let Some(mnemonic) = line.strip_prefix('@') {
        self.ptr += 1;

        match mnemonic.parse::<u16>() {
          Ok(value) => return Some(Token::A(AToken::Literal(value))),
          Err(_) => {
            if let Some(address) = INTRINSICS.get(mnemonic) {
              return Some(Token::A(AToken::Literal(*address)));
            } else if let Some(address) = self.symbol_table.get(mnemonic) {
              return Some(Token::A(AToken::Mnemonic(Rc::clone(address))));
            } else {
              let address = Rc::new(Cell::new(0));

              self
                .symbol_table
                .insert(mnemonic.to_string(), Rc::clone(&address));
              self
                .address_order
                .insert(mnemonic.to_string(), address.clone());

              return Some(Token::A(AToken::Mnemonic(address)));
            }
          }
        };
      }
    }

    return None;
  }

  fn parse_c_token(
    &mut self,
    line: &str,
    raw_line: &str,
    raw_line_len: usize,
  ) -> Option<Token> {
    let mut cmp_token: Option<&str> = None;

    let dst = if line.contains('=') {
      if let Some((dst, cmp)) = line.split_once('=') {
        if dst.is_empty() {
          error_panic_fmt_file!(
            &self.file_info.name,
            &self.file_info.content,
            self.pos
              - (raw_line_len
                - raw_line.find_ignoring_whitespaces('=').unwrap_or(0)),
            "(ParseError)",
            "No destination found"
          );
        } else if cmp.is_empty() {
          error_panic_fmt_file!(
            &self.file_info.name,
            &self.file_info.content,
            self.pos
              - (raw_line_len
                - raw_line.find_ignoring_whitespaces('=').unwrap_or(0)),
            "(ParseError)",
            "No assignment instruction found"
          );
        }

        cmp_token = Some(cmp);

        CDestinationToken {
          a_reg: dst.contains('A'),
          d_reg: dst.contains('D'),
          ram: dst.contains('M'),
        }
      } else {
        error_panic_src!("Could not split destination and comparison");
      }
    } else {
      CDestinationToken {
        a_reg: false,
        d_reg: false,
        ram: false,
      }
    };

    let jmp = if line.contains(';') {
      if let Some((cmp, jmp)) = line.split_once(';') {
        if cmp.is_empty() {
          error_panic_fmt_file!(
            &self.file_info.name,
            &self.file_info.content,
            self.pos
              - (raw_line_len
                - raw_line.find_ignoring_whitespaces(';').unwrap_or(0)),
            "(ParseError)",
            "No value found"
          );
        } else if jmp.is_empty() {
          error_panic_fmt_file!(
            &self.file_info.name,
            &self.file_info.content,
            self.pos
              - (raw_line_len
                - raw_line.find_ignoring_whitespaces(';').unwrap_or(0)),
            "(ParseError)",
            "No jump instruction found"
          );
        }

        cmp_token = Some(cmp);

        self.match_jump_token(Some(jmp), raw_line, raw_line_len)
      } else {
        error_panic_src!("Could not split comparison and jump");
      }
    } else {
      CJumpToken {
        eq: false,
        gt: false,
        lt: false,
      }
    };

    if cmp_token.is_none() {
      cmp_token = Some(line);
    }

    let cmp = self.match_comp_token(cmp_token, raw_line, raw_line_len);

    self.ptr += 1;

    return Some(Token::C(CToken { cmp, dst, jmp }));
  }
}

impl<'a> Iterator for AsmParser<'a> {
  type Item = Token;

  fn next(&mut self) -> Option<Self::Item> {
    if let Some(raw_line) = self.file_lines.next() {
      let raw_line_len = raw_line.len();
      self.pos += raw_line_len + 1; // +1 for newlines

      let line: String = raw_line
        .split("//")
        .next()
        .expect(error_fmt_src!("Invalid line"))
        .remove_whitespaces();

      if line.is_empty() {
        return self.next();
      }

      if let Some(token) = self.parse_label(&line, raw_line, raw_line_len) {
        return Some(token);
      }

      if let Some(token) = self.parse_a_token(&line) {
        return Some(token);
      }

      if let Some(token) = self.parse_c_token(&line, raw_line, raw_line_len) {
        return Some(token);
      }
    }

    self.assign_addresses();

    if Setting::PrintAll.is_set().is_ok()
      || Setting::PrintSymbols.is_set().is_ok()
    {
      info_print!("Symbol Table");
      println!("{:#?}", self.symbol_table);
    }

    return None;
  }
}
