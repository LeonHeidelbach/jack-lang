////////////////////////////////////////////////////////////////////////////////
// File: src/internal/parse.rs
// Description: Parser module
//
// Author: Leon Heidelbach <leon.heidelbach@hhu.de>
// Date: 06.05.2024
//
// License: GPLv3
////////////////////////////////////////////////////////////////////////////////

use std::{
  ops::AddAssign,
  str::{Lines, SplitWhitespace},
};

use super::{intermediate::IntermediateRepresentation, vm::*};

use anyhow::Result;
use shared::{
  error_fmt_file, error_fmt_src, error_panic_fmt_file, info_print,
  io::os::FileInfo,
  util::{helpers::FindIgnoringWhitespace, settings::Setting},
};

////////////////////////////////////////////////////////////////////////////////
// Program
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
pub(crate) struct Program<'a> {
  pub(crate) file_info: &'a FileInfo,
  pub(crate) tokens: Vec<Token>,
  pub(crate) instructions: String,
  pub(crate) label_id: usize,
}

impl<'a> AddAssign<&'a Program<'a>> for Program<'a> {
  fn add_assign(&mut self, rhs: &'a Self) {
    self.tokens.extend(rhs.tokens.clone());
    self.instructions.push_str(&rhs.instructions);
  }
}

impl<'a> Program<'a> {
  pub(crate) fn new(file_info: &'a FileInfo, tokens: Vec<Token>) -> Self {
    return Self {
      file_info,
      tokens,
      instructions: String::new(),
      label_id: 0,
    };
  }

  pub(crate) fn from_token_stream(
    tokens: Vec<Token>,
    file_info: &'a FileInfo,
  ) -> Self {
    let mut _self = Self {
      tokens,
      instructions: String::new(),
      label_id: 0,
      file_info,
    };

    _self.generate_instructions();

    return _self;
  }

  pub(crate) fn fold_into<'b>(&mut self, others: &'a [Program<'b>]) {
    self.insert_init();
    self.generate_instructions();

    others.iter().fold(self, |acc, x| {
      *acc += x;
      return acc;
    });
  }

  pub(crate) fn generate_instructions(&mut self) {
    for token in self.tokens.clone().into_iter() {
      match token {
        Token::Push(push) => push.intermediate(self),
        Token::Pop(pop) => pop.intermediate(self),
        Token::Goto(go_to) => go_to.intermediate(self),
        Token::IfGoto(if_go_to) => if_go_to.intermediate(self),
        Token::Label(label) => label.intermediate(self),
        Token::FunctionDecl(function_decl) => function_decl.intermediate(self),
        Token::FunctionCall(function_call) => function_call.intermediate(self),
        Token::Add(add) => add.intermediate(self),
        Token::Sub(sub) => sub.intermediate(self),
        Token::Neg(neg) => neg.intermediate(self),
        Token::Eq(eq) => eq.intermediate(self),
        Token::Gt(gt) => gt.intermediate(self),
        Token::Lt(lt) => lt.intermediate(self),
        Token::And(and) => and.intermediate(self),
        Token::Or(or) => or.intermediate(self),
        Token::Not(not) => not.intermediate(self),
        Token::Return(ret) => ret.intermediate(self),
      }
    }
  }

  pub(crate) fn insert_init(&mut self) {
    self.push_comment("INITIALIZATION");
    self.push_const_to_d(256);
    self.push_d_to_ptr_val("SP");
    self.tokens.insert(
      0,
      Token::FunctionCall(FunctionCall(
        Segment::FunctionName("Sys.init".to_string()),
        0,
      )),
    );
  }

  pub(crate) fn push(&mut self, instruction: &str) {
    self.instructions.push_str(instruction);
    self.instructions.push('\n');
  }

  pub(crate) fn push_comment(&mut self, comment: &str) {
    self.push(&format!("// --- {} --- //", comment));
  }

  pub(crate) fn push_inc_sp(&mut self) {
    self.push("@SP");
    self.push("M=M+1");
  }

  pub(crate) fn push_dec_sp(&mut self) {
    self.push("@SP");
    self.push("M=M-1");
  }

  pub(crate) fn push_d_to_stack(&mut self) {
    self.push("@SP");
    self.push("A=M");
    self.push("M=D");
  }

  pub(crate) fn push_stack_to_d(&mut self) {
    self.push("@SP");
    self.push("A=M");
    self.push("D=M");
  }

  pub(crate) fn push_ptr_val_to_d(&mut self, addr: &str) {
    self.push(&format!("@{}", addr));
    self.push("D=M");
  }

  pub(crate) fn push_d_to_ptr_val(&mut self, addr: &str) {
    self.push(&format!("@{}", addr));
    self.push("M=D");
  }

  pub(crate) fn push_const_to_d(&mut self, value: u16) {
    self.push(&format!("@{}", value));
    self.push("D=A");
  }

  pub(crate) fn push_label(&mut self, label: &str) {
    self.push_comment(&format!("LABEL: {}", label));
    self.push(&format!("({})", label));
  }

  pub(crate) fn push_goto(&mut self, label: &str) {
    self.push_comment(&format!("GOTO: {}", label));
    self.push(&format!("@{}", label));
    self.push("0;JMP");
  }

  pub(crate) fn push_if_goto(&mut self, label: &str) {
    self.push_comment(&format!("IF-GOTO: {}", label));

    // Pop the stack and jump if not zero
    self.push_dec_sp();
    self.push_stack_to_d();
    self.push(&format!("@{}", label));
    self.push("D;JNE");
  }

  pub(crate) fn push_comp_stack_args_1(&mut self, instr: &str, comment: &str) {
    self.push_comment(comment);

    // Read the top stack value and apply the instruction
    self.push("@SP");
    self.push("A=M-1");
    self.push(&format!("M={}", instr));
  }

  pub(crate) fn push_comp_stack_args_2(&mut self, instr: &str, comment: &str) {
    self.push_comment(comment);

    // Read the top two stack values and apply the instruction
    self.push("@SP");
    self.push("AM=M-1");
    self.push("D=M");
    self.push("A=A-1");
    self.push(&format!("M={}", instr));
  }

  pub(crate) fn push_comparison(&mut self, instr: &str, comment: &str) {
    // 1. Subtract the top two stack valuesa and store the result in D
    self.push_comp_stack_args_2("M-D", comment);
    self.push("D=M");

    // 2. Jump to the IF_TRUE label if the comparison is true using the given instruction
    self.push(&format!(
      "@__IF_TRUE_{}_{}_{}",
      instr, self.file_info.stem, self.label_id
    ));
    self.push(&format!("D;{}", instr));

    // 3. Set the top stack value to 0 if the comparison fell through i.e. result is false
    self.push("@SP");
    self.push("A=M-1");
    self.push("M=0");

    // 4. Jump to the end of the comparison
    self.push(&format!(
      "@__IF_FALSE_{}_{}_{}",
      instr, self.file_info.stem, self.label_id
    ));
    self.push("0;JMP");

    // 5. Create a unique IF_TRUE label for this comparison
    self.push_label(&format!(
      "__IF_TRUE_{}_{}_{}",
      instr, self.file_info.stem, self.label_id
    ));

    // 5. Set the top stack value to -1 if the comparison is true
    self.push("@SP");
    self.push("A=M-1");
    self.push("M=-1");

    // 6. Create a unique IF_FALSE label for this comparison
    self.push_label(&format!(
      "__IF_FALSE_{}_{}_{}",
      instr, self.file_info.stem, self.label_id
    ));

    // Increment the label id to avoid namespace collisions
    self.label_id += 1;
  }
}

////////////////////////////////////////////////////////////////////////////////
// VM Instruction Parser
////////////////////////////////////////////////////////////////////////////////

pub(crate) struct VmParser<'a> {
  pos: usize,
  file_info: &'a FileInfo,
  file_lines: Lines<'a>,
}

impl<'a> VmParser<'a> {
  // Segment names
  const SEGMENT_ARG: &'static str = "argument";
  const SEGMENT_CONST: &'static str = "constant";
  const SEGMENT_LOCAL: &'static str = "local";
  const SEGMENT_TEMP: &'static str = "temp";
  const SEGMENT_THAT: &'static str = "that";
  const SEGMENT_THIS: &'static str = "this";
  const SEGMENT_STATIC: &'static str = "static";
  const SEGMENT_PTR: &'static str = "pointer";

  // Instruction names
  const INSTR_PUSH: &'static str = "push";
  const INSTR_POP: &'static str = "pop";
  const INSTR_GOTO: &'static str = "goto";
  const INSTR_IF_GOTO: &'static str = "if-goto";
  const INSTR_LABEL: &'static str = "label";
  const INSTR_FUNCTION: &'static str = "function";
  const INSTR_CALL: &'static str = "call";
  const INSTR_ADD: &'static str = "add";
  const INSTR_SUB: &'static str = "sub";
  const INSTR_NEG: &'static str = "neg";
  const INSTR_EQ: &'static str = "eq";
  const INSTR_GT: &'static str = "gt";
  const INSTR_LT: &'static str = "lt";
  const INSTR_AND: &'static str = "and";
  const INSTR_OR: &'static str = "or";
  const INSTR_NOT: &'static str = "not";
  const INSTR_RETURN: &'static str = "return";

  pub(crate) fn parse(file_info: &'a FileInfo) -> Result<Program> {
    info_print!("Parsing file: `{}`", file_info.name);

    let parser = Self {
      pos: 0,
      file_info,
      file_lines: file_info.content.lines(),
    };

    let program = Program::from_token_stream(parser.collect(), file_info);

    if Setting::PrintAll.is_set().is_ok()
      || Setting::PrintProgram.is_set().is_ok()
    {
      info_print!("Program");
      println!("{:#?}", program);
    }

    return Ok(program);
  }

  fn match_segment(&self, segment: &str, instr_name: &str) -> Option<Segment> {
    match segment {
      Self::SEGMENT_ARG => return Some(Segment::Argument),
      Self::SEGMENT_CONST => return Some(Segment::Constant),
      Self::SEGMENT_LOCAL => return Some(Segment::Local),
      Self::SEGMENT_TEMP => return Some(Segment::Temp),
      Self::SEGMENT_THAT => return Some(Segment::That),
      Self::SEGMENT_THIS => return Some(Segment::This),
      Self::SEGMENT_STATIC => return Some(Segment::Static),
      Self::SEGMENT_PTR => return Some(Segment::Pointer),
      _ => match instr_name {
        Self::INSTR_FUNCTION | Self::INSTR_CALL => {
          return Some(Segment::FunctionName(segment.to_string()))
        }
        _ => return None,
      },
    }
  }

  fn parse_instruction_params(
    &self,
    parts: &mut SplitWhitespace<'_>,
    instr_name: &str,
    raw_line: &str,
    raw_line_len: usize,
  ) -> (Segment, u16) {
    let seg_str = parts.next().expect(error_fmt_file!(
      &self.file_info.name,
      &self.file_info.content,
      self.pos
        - (raw_line_len
          - raw_line.find_ignoring_whitespaces(instr_name).unwrap_or(0)),
      "(ParseError)",
      "Segment missing in {} instruction",
      instr_name
    ));
    let segment =
      self
        .match_segment(seg_str, instr_name)
        .expect(error_fmt_file!(
          &self.file_info.name,
          &self.file_info.content,
          self.pos
            - (raw_line_len
              - raw_line.find_ignoring_whitespaces(seg_str).unwrap_or(0)),
          "(ParseError)",
          "Unknown segment instruction: `{}`",
          seg_str
        ));
    let value = parts
      .next()
      .expect(error_fmt_file!(
        &self.file_info.name,
        &self.file_info.content,
        self.pos
          - (raw_line_len
            - raw_line.find_ignoring_whitespaces(seg_str).unwrap_or(0)),
        "(ParseError)",
        "Value missing in {} instruction",
        instr_name
      ))
      .parse::<u16>()
      .expect(error_fmt_file!(
        &self.file_info.name,
        &self.file_info.content,
        self.pos
          - (raw_line_len
            - raw_line.find_ignoring_whitespaces(seg_str).unwrap_or(0)),
        "(ParseError)",
        "Value has to be a number of type u16"
      ));

    return (segment, value);
  }
}

impl<'a> Iterator for VmParser<'a> {
  type Item = Token;

  fn next(&mut self) -> Option<Self::Item> {
    if let Some(raw_line) = self.file_lines.next() {
      let raw_line_len = raw_line.len();
      self.pos += raw_line_len + 1; // +1 for the newlines

      let line: String = raw_line
        .split("//")
        .next()
        .expect(error_fmt_src!("Invalid line"))
        .trim_start()
        .trim_end()
        .to_string();

      if line.is_empty() {
        return self.next();
      }

      let mut line_parts = line.split_whitespace();
      let instr_str = line_parts.next().expect(error_fmt_file!(
        &self.file_info.name,
        &self.file_info.content,
        self.pos - raw_line_len,
        "(ParseError)",
        "No instruction found in line"
      ));

      match instr_str {
        Self::INSTR_PUSH => {
          let (segment, value) = self.parse_instruction_params(
            &mut line_parts,
            instr_str,
            raw_line,
            raw_line_len,
          );

          return Some(Token::Push(Push(segment, value)));
        }
        Self::INSTR_POP => {
          let (segment, value) = self.parse_instruction_params(
            &mut line_parts,
            instr_str,
            raw_line,
            raw_line_len,
          );

          return Some(Token::Pop(Pop(segment, value)));
        }
        Self::INSTR_GOTO => {
          let label = line_parts.next().expect(error_fmt_file!(
            &self.file_info.name,
            &self.file_info.content,
            self.pos
              - (raw_line_len
                - line.find_ignoring_whitespaces("goto").unwrap_or(0)),
            "(ParseError)",
            "Label missing in goto instruction"
          ));

          return Some(Token::Goto(Goto(label.to_string())));
        }
        Self::INSTR_IF_GOTO => {
          let label = line_parts.next().expect(error_fmt_file!(
            &self.file_info.name,
            &self.file_info.content,
            self.pos
              - (raw_line_len
                - line.find_ignoring_whitespaces("goto").unwrap_or(0)),
            "(ParseError)",
            "Label missing in if-goto instruction"
          ));

          return Some(Token::IfGoto(IfGoto(label.to_string())));
        }
        Self::INSTR_LABEL => {
          let label = line_parts.next().expect(error_fmt_file!(
            &self.file_info.name,
            &self.file_info.content,
            self.pos
              - (raw_line_len
                - line.find_ignoring_whitespaces("goto").unwrap_or(0)),
            "(ParseError)",
            "Label name missing in label instruction"
          ));

          return Some(Token::Label(Label(label.to_string())));
        }
        Self::INSTR_FUNCTION => {
          let (segment, value) = self.parse_instruction_params(
            &mut line_parts,
            instr_str,
            raw_line,
            raw_line_len,
          );

          return Some(Token::FunctionDecl(FunctionDecl(segment, value)));
        }
        Self::INSTR_CALL => {
          let (segment, value) = self.parse_instruction_params(
            &mut line_parts,
            instr_str,
            raw_line,
            raw_line_len,
          );

          return Some(Token::FunctionCall(FunctionCall(segment, value)));
        }
        Self::INSTR_ADD => return Some(Token::Add(Add)),
        Self::INSTR_SUB => return Some(Token::Sub(Sub)),
        Self::INSTR_NEG => return Some(Token::Neg(Neg)),
        Self::INSTR_EQ => return Some(Token::Eq(Eq)),
        Self::INSTR_GT => return Some(Token::Gt(Gt)),
        Self::INSTR_LT => return Some(Token::Lt(Lt)),
        Self::INSTR_AND => return Some(Token::And(And)),
        Self::INSTR_OR => return Some(Token::Or(Or)),
        Self::INSTR_NOT => return Some(Token::Not(Not)),
        Self::INSTR_RETURN => return Some(Token::Return(Return)),
        _ => {
          error_panic_fmt_file!(
            &self.file_info.name,
            &self.file_info.content,
            self.pos - raw_line_len,
            "(ParseError)",
            "Unknown instruction: `{}`",
            instr_str
          );
        }
      }
    }

    return None;
  }
}
