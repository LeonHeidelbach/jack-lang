////////////////////////////////////////////////////////////////////////////////
// File: src/internal/intermediate.rs
// Description: Intermediate representation module
//
// Author: Leon Heidelbach <leon.heidelbach@hhu.de>
// Date: 06.05.2024
//
// License: GPLv3
////////////////////////////////////////////////////////////////////////////////

use super::{parse::Program, vm::*};

use shared::error_panic_src;
use shared::util::traits::Serializable;

////////////////////////////////////////////////////////////////////////////////
// Constants
////////////////////////////////////////////////////////////////////////////////

const TEMP_SEGMENT_OFFSET: u16 = 5;

////////////////////////////////////////////////////////////////////////////////
// Traits
////////////////////////////////////////////////////////////////////////////////

pub trait IntermediateRepresentation<'a> {
  type Program: Serializable;
  fn intermediate(&self, prog: &mut Self::Program);
}

////////////////////////////////////////////////////////////////////////////////
// Two Argument Instructions
////////////////////////////////////////////////////////////////////////////////

impl<'a> IntermediateRepresentation<'a> for Push {
  type Program = Program<'a>;
  fn intermediate(&self, prog: &mut Self::Program) {
    let Push(segment, value) = self;

    prog.push_comment(&format!("PUSH: {} {}", segment, value));

    // Push value onto stack
    match segment {
      Segment::This | Segment::That | Segment::Argument | Segment::Local => {
        // Calculate address in THIS/THAT/ARGUMENT/LOCAL segment
        prog.push(&format!("@{}", segment));
        prog.push("D=M");
        prog.push(&format!("@{}", value));
        prog.push("A=D+A");
        prog.push("D=M");

        // Push to stack
        prog.push_d_to_stack();
        prog.push_inc_sp();
      }
      Segment::Static => {
        // Load value from static segment
        prog.push(&format!("@{}.{}", prog.file_info.stem, value));
        prog.push("D=M");

        // Push to stack
        prog.push_d_to_stack();
        prog.push_inc_sp();
      }
      Segment::Constant => {
        // Push constant value to d
        prog.push_const_to_d(*value);

        // Push to stack
        prog.push_d_to_stack();
        prog.push_inc_sp();
      }
      Segment::Temp => {
        // Calculate address in temp segment
        prog.push(&format!("@{}", TEMP_SEGMENT_OFFSET + value));
        prog.push("D=M");

        // Push to stack
        prog.push_d_to_stack();
        prog.push_inc_sp();
      }
      Segment::Pointer => {
        // Calculate address in pointer segment
        let ptr_offset = match value {
          0 => Segment::This,
          1 => Segment::That,
          _ => {
            error_panic_src!(
              "(ParseError) Invalid pointer offset value `{}`",
              value
            )
          }
        };

        // Load value from pointer segment
        prog.push(&format!("@{}", ptr_offset));
        prog.push("D=M");

        // Push to stack
        prog.push_d_to_stack();
        prog.push_inc_sp();
      }
      _ => {
        error_panic_src!("(ParseError) Invalid segment for push: `{}`", segment)
      }
    }
  }
}

impl<'a> IntermediateRepresentation<'a> for Pop {
  type Program = Program<'a>;
  fn intermediate(&self, prog: &mut Self::Program) {
    let Pop(segment, value) = self;

    prog.push_comment(&format!("POP: {} {}", segment, value));

    // Pop value from segment into D register
    match segment {
      Segment::This | Segment::That | Segment::Argument | Segment::Local => {
        // Calculate address in THIS/THAT/ARGUMENT/LOCAL segment
        prog.push_ptr_val_to_d(segment.as_str());
        prog.push(&format!("@{}", value));
        prog.push("D=D+A");
        prog.push_d_to_ptr_val("R13");

        // Pop value into D
        prog.push_dec_sp();
        prog.push_stack_to_d();
        prog.push("@R13");
        prog.push("A=M");
        prog.push("M=D");
      }
      Segment::Static => {
        // Pop static value into D
        prog.push_dec_sp();
        prog.push_stack_to_d();
        prog.push(&format!("@{}.{}", prog.file_info.stem, value));
        prog.push("M=D");
      }
      Segment::Temp => {
        // Calculate address in temp segment
        prog.push_const_to_d(TEMP_SEGMENT_OFFSET);
        prog.push(&format!("@{}", value));
        prog.push("D=D+A");
        prog.push_d_to_ptr_val("R13");

        // Pop value into D
        prog.push_dec_sp();
        prog.push_stack_to_d();
        prog.push("@R13");
        prog.push("A=M");
        prog.push("M=D");
      }
      Segment::Pointer => {
        // Calculate address in pointer segment
        let ptr_offset = match value {
          0 => Segment::This,
          1 => Segment::That,
          _ => {
            error_panic_src!(
              "(ParseError) Invalid pointer offset value `{}`",
              value
            )
          }
        };

        // Pop value into D
        prog.push_dec_sp();
        prog.push_stack_to_d();
        prog.push(&format!("@{}", ptr_offset));
        prog.push("M=D");
      }
      _ => {
        error_panic_src!("(ParseError) Invalid segment for pop: `{}`", segment)
      }
    }
  }
}

impl<'a> IntermediateRepresentation<'a> for FunctionDecl {
  type Program = Program<'a>;
  fn intermediate(&self, prog: &mut Self::Program) {
    let FunctionDecl(segment, value) = self;

    prog.push_comment(&format!("FUNCTION: {} {}", segment, value));

    // Add function label
    prog.push_label(&format!("__CALL_{}_", segment));

    // Initialize local variables
    for _ in 0..*value {
      prog.push_const_to_d(0);
      prog.push_d_to_stack();
      prog.push_inc_sp();
    }
  }
}

impl<'a> IntermediateRepresentation<'a> for FunctionCall {
  type Program = Program<'a>;
  fn intermediate(&self, prog: &mut Self::Program) {
    let FunctionCall(segment, value) = self;

    prog.push_comment(&format!("CALL: {} {}", segment, value));

    // 1. Save return address
    prog.push(&format!("@__CALL_{}_{}.$RET", segment, prog.label_id));
    prog.push("D=A");
    prog.push_d_to_stack();
    prog.push_inc_sp();

    // 2. Save call frame
    prog.push_ptr_val_to_d(Segment::Local.as_str());
    prog.push_d_to_stack();
    prog.push_inc_sp();

    // 3. Save ARGUMENT frame
    prog.push_ptr_val_to_d(Segment::Argument.as_str());
    prog.push_d_to_stack();
    prog.push_inc_sp();

    // 4. Save THIS frame
    prog.push_ptr_val_to_d(Segment::This.as_str());
    prog.push_d_to_stack();
    prog.push_inc_sp();

    // 5. Save THAT frame
    prog.push_ptr_val_to_d(Segment::That.as_str());
    prog.push_d_to_stack();
    prog.push_inc_sp();

    // 6. Set new ARGUMENT frame
    prog.push_ptr_val_to_d("SP");
    prog.push("@5");
    prog.push("D=D-A");
    prog.push(&format!("@{}", value));
    prog.push("D=D-A");
    prog.push_d_to_ptr_val(Segment::Argument.as_str());

    // 7. Reposition LCL
    prog.push_ptr_val_to_d("SP");
    prog.push_d_to_ptr_val(Segment::Local.as_str());

    // 8. Jump to function
    prog.push(&format!("@__CALL_{}_", segment));
    prog.push("0;JMP");

    // 9. Add return label
    prog.push_label(&format!("__CALL_{}_{}.$RET", segment, prog.label_id));

    // 10. Increment label id to prevent namespace collisions
    prog.label_id += 1;
  }
}

////////////////////////////////////////////////////////////////////////////////
// One Argument Instructions
////////////////////////////////////////////////////////////////////////////////

impl<'a> IntermediateRepresentation<'a> for Goto {
  type Program = Program<'a>;
  fn intermediate(&self, prog: &mut Self::Program) {
    let Goto(label) = self;
    return prog.push_goto(label);
  }
}

impl<'a> IntermediateRepresentation<'a> for IfGoto {
  type Program = Program<'a>;
  fn intermediate(&self, prog: &mut Self::Program) {
    let IfGoto(label) = self;
    return prog.push_if_goto(label);
  }
}

impl<'a> IntermediateRepresentation<'a> for Label {
  type Program = Program<'a>;
  fn intermediate(&self, prog: &mut Self::Program) {
    let Label(label) = self;
    return prog.push_label(label);
  }
}

////////////////////////////////////////////////////////////////////////////////
// Zero Argument Instructions
////////////////////////////////////////////////////////////////////////////////

impl<'a> IntermediateRepresentation<'a> for Return {
  type Program = Program<'a>;
  fn intermediate(&self, prog: &mut Self::Program) {
    prog.push_comment("RETURN");

    // 1. Save return value
    prog.push_ptr_val_to_d(Segment::Local.as_str());
    prog.push_d_to_ptr_val("R13");
    prog.push("@5");
    prog.push("A=D-A");
    prog.push("D=M");
    prog.push_d_to_ptr_val("R14");

    // 2. Pop return value to start of frame
    prog.push_dec_sp();
    prog.push_stack_to_d();
    prog.push("@ARG");
    prog.push("A=M");
    prog.push("M=D");

    // 3. Restore SP
    prog.push("@ARG");
    prog.push("D=M+1");
    prog.push_d_to_ptr_val("SP");

    // 4. Restore call frame
    // Restore THAT
    prog.push_ptr_val_to_d("R13");
    prog.push("@1");
    prog.push("D=D-A");
    prog.push("A=D");
    prog.push("D=M");
    prog.push("@THAT");
    prog.push("M=D");

    // Restore THIS
    prog.push_ptr_val_to_d("R13");
    prog.push("@2");
    prog.push("D=D-A");
    prog.push("A=D");
    prog.push("D=M");
    prog.push("@THIS");
    prog.push("M=D");

    // Restore ARG
    prog.push_ptr_val_to_d("R13");
    prog.push("@3");
    prog.push("D=D-A");
    prog.push("A=D");
    prog.push("D=M");
    prog.push("@ARG");
    prog.push("M=D");

    // Restore LCL
    prog.push_ptr_val_to_d("R13");
    prog.push("@4");
    prog.push("D=D-A");
    prog.push("A=D");
    prog.push("D=M");
    prog.push("@LCL");
    prog.push("M=D");

    // 5. Jump to return address
    prog.push_ptr_val_to_d("R14");
    prog.push("A=D");
    prog.push("0;JMP");
  }
}

impl<'a> IntermediateRepresentation<'a> for Add {
  type Program = Program<'a>;
  fn intermediate(&self, prog: &mut Self::Program) {
    return prog.push_comp_stack_args_2("D+M", "ADD Operation");
  }
}

impl<'a> IntermediateRepresentation<'a> for Sub {
  type Program = Program<'a>;
  fn intermediate(&self, prog: &mut Self::Program) {
    return prog.push_comp_stack_args_2("M-D", "SUB Operation");
  }
}
impl<'a> IntermediateRepresentation<'a> for Neg {
  type Program = Program<'a>;
  fn intermediate(&self, prog: &mut Self::Program) {
    return prog.push_comp_stack_args_1("-M", "NEG Operation");
  }
}
impl<'a> IntermediateRepresentation<'a> for Eq {
  type Program = Program<'a>;
  fn intermediate(&self, prog: &mut Self::Program) {
    return prog.push_comparison("JEQ", "EQ Operation");
  }
}
impl<'a> IntermediateRepresentation<'a> for Gt {
  type Program = Program<'a>;
  fn intermediate(&self, prog: &mut Self::Program) {
    return prog.push_comparison("JGT", "GT Operation");
  }
}
impl<'a> IntermediateRepresentation<'a> for Lt {
  type Program = Program<'a>;
  fn intermediate(&self, prog: &mut Self::Program) {
    return prog.push_comparison("JLT", "LT Operation");
  }
}
impl<'a> IntermediateRepresentation<'a> for And {
  type Program = Program<'a>;
  fn intermediate(&self, prog: &mut Self::Program) {
    return prog.push_comp_stack_args_2("D&M", "AND Operation");
  }
}
impl<'a> IntermediateRepresentation<'a> for Or {
  type Program = Program<'a>;
  fn intermediate(&self, prog: &mut Self::Program) {
    return prog.push_comp_stack_args_2("D|M", "OR Operation");
  }
}

impl<'a> IntermediateRepresentation<'a> for Not {
  type Program = Program<'a>;
  fn intermediate(&self, prog: &mut Self::Program) {
    return prog.push_comp_stack_args_1("!M", "NOT Operation");
  }
}
