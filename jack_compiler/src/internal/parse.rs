////////////////////////////////////////////////////////////////////////////////
// File: src/internal/parse.rs
// Description: Jack Compiler parsing module
//
// Author: Leon Heidelbach <leon.heidelbach@hhu.de>
// Date: 15.05.2024
//
// License: GPLv3
////////////////////////////////////////////////////////////////////////////////

use crate::internal::ast::Ast;

use super::ast::Program;

use anyhow::Result;
use shared::{info_print, io::os::FileInfo, util::settings::Setting};

////////////////////////////////////////////////////////////////////////////////
// Jack Parser
////////////////////////////////////////////////////////////////////////////////

pub(crate) struct JackParser;

impl JackParser {
  pub(crate) fn parse(file_info: &FileInfo) -> Result<Program> {
    info_print!("Parsing file: `{}`", file_info.name);

    let program = Ast::parse(file_info)?.program;

    if Setting::PrintAll.is_set().is_ok()
      || Setting::PrintProgram.is_set().is_ok()
    {
      info_print!("Program");
      println!("{:#?}", program);
    }

    return Ok(program);
  }
}
