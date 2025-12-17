////////////////////////////////////////////////////////////////////////////////
// File: src/internal/compile.rs
// Description: Compilation module
//
// Author: Leon Heidelbach <leon.heidelbach@hhu.de>
// Date: 01.05.2024
//
// License: GPLv3
////////////////////////////////////////////////////////////////////////////////

use super::parse::AsmParser;

use shared::{
  error_panic_src, green, info_print,
  io::{
    cli::{CommandLineParser, Token},
    os::{generate_output, out_dir_check, read_file_list, FileInfo},
  },
  util::parse::parse_programs,
};

////////////////////////////////////////////////////////////////////////////////
// Constants
////////////////////////////////////////////////////////////////////////////////

const ASM_EXTENSION: &str = "asm";
const HACK_EXTENSION: &str = "hack";

////////////////////////////////////////////////////////////////////////////////
// Compilation Step
////////////////////////////////////////////////////////////////////////////////

pub(crate) fn compile(_c: &mut CommandLineParser, t: &mut Token) {
  let files: Vec<FileInfo> = read_file_list(
    &t.parameters.clone().unwrap_or_else(|| {
      error_panic_src!("(ArgError) No input files provided!")
    }),
    ASM_EXTENSION,
  );

  let programs = parse_programs(&files, AsmParser::parse);
  let output_dir = out_dir_check();

  generate_output(&programs, &files, output_dir.as_deref(), HACK_EXTENSION);

  info_print!("{}", green!("Compilation successful!", true, false));
}
