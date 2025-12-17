////////////////////////////////////////////////////////////////////////////////
// File: src/internal/compile.rs
// Description: Compilation module
//
// Author: Leon Heidelbach <leon.heidelbach@hhu.de>
// Date: 06.05.2024
//
// License: GPLv3
////////////////////////////////////////////////////////////////////////////////

use std::path::PathBuf;

use crate::internal::parse::{Program, VmParser};

use shared::{
  error_fmt, error_panic, error_panic_src, green, info_print,
  io::{
    cli::{CommandLineParser, Token},
    os::{generate_output, out_dir_check, read_file_list, FileInfo},
  },
  util::{parse::parse_programs, settings::Setting},
};

////////////////////////////////////////////////////////////////////////////////
// Constants
////////////////////////////////////////////////////////////////////////////////

const VM_EXTENSION: &str = "vm";
const ASM_EXTENSION: &str = "asm";

////////////////////////////////////////////////////////////////////////////////
// Compilation Step
////////////////////////////////////////////////////////////////////////////////

pub(crate) fn compile(_c: &mut CommandLineParser, t: &mut Token) {
  let files: Vec<FileInfo> = read_file_list(
    &t.parameters.clone().unwrap_or_else(|| {
      error_panic_src!("(ArgError) No input files provided!")
    }),
    VM_EXTENSION,
  );

  let programs = parse_programs(&files, VmParser::parse);

  if let Ok(Some(values)) = Setting::Output.is_set() {
    let out_file = values
      .first()
      .expect(error_fmt!("(ArgError) No output file provided!"));
    let path = PathBuf::from(out_file);

    if let Some(ext) = path.extension() {
      if ext != ASM_EXTENSION {
        error_panic!(
          "The provided output file path does not have the \
          correct file extension (\".asm\")! (Found: \".{}\")",
          ext.to_str().unwrap()
        );
      }

      let file_info = FileInfo::new(out_file.to_string(), path, String::new());
      let mut program = Program::new(&file_info, Vec::new());
      program.fold_into(&programs);

      generate_output(&[program], &[file_info.clone()], None, ASM_EXTENSION);

      info_print!("{}", green!("Compilation successful!", true, false));

      return;
    }
  }

  let output_dir = out_dir_check();
  generate_output(&programs, &files, output_dir.as_deref(), ASM_EXTENSION);

  info_print!("{}", green!("Compilation successful!", true, false));
}
