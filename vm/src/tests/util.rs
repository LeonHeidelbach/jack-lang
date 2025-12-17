////////////////////////////////////////////////////////////////////////////////
// File: src/tests/util.rs
// Description: Test utility functions
//
// Author: Leon Heidelbach <leon.heidelbach@hhu.de>
// Date: 06.05.2024
//
// License: GPLv3
////////////////////////////////////////////////////////////////////////////////

use std::{fs, path::PathBuf};

use crate::internal::parse::{Program, VmParser};

use shared::{
  error_fmt, error_panic,
  io::os::{generate_output, read_file_list, FileInfo},
  util::parse::parse_programs,
};

////////////////////////////////////////////////////////////////////////////////
// Constants
////////////////////////////////////////////////////////////////////////////////

pub(super) const VM_FILES_P7: &str = "src/tests/7/";
pub(super) const VM_FILES_P8: &str = "src/tests/8/";
pub(super) const CPU_EMULATOR_TOOL_PATH: &str = "../tools/CPUEmulator.sh";

////////////////////////////////////////////////////////////////////////////////
// Test Helper Functions
////////////////////////////////////////////////////////////////////////////////

pub(super) fn run_cpu_emulator(file_path: &str) {
  let output = std::process::Command::new(CPU_EMULATOR_TOOL_PATH)
    .arg(file_path)
    .output()
    .expect("failed to execute process");

  let stdout = String::from_utf8_lossy(&output.stdout);
  let stderr = String::from_utf8_lossy(&output.stderr);

  if !output.status.success() {
    error_panic!(
      "Command failed: \n\tstout: {}\n\tstderr: {}",
      stdout,
      stderr
    );
  }

  assert_eq!(stdout, "End of script - Comparison ended successfully\n");
}

pub(super) fn evaluate_single_input_test(files: &[String]) {
  let files = read_file_list(files, "vm");
  let programs = parse_programs(&files, VmParser::parse);
  let output: Vec<(PathBuf, String)> =
    generate_output(&programs, &files, None, "asm");

  output.iter().for_each(|(path, _)| {
    let mut tst_path = path.clone();
    tst_path.set_extension("tst");
    run_cpu_emulator(tst_path.to_str().unwrap());

    fs::remove_file(path).expect(error_fmt!(
      "(IOError) Failed to remove file: `{}`",
      path.to_str().expect(error_fmt!("(ArgError) Invalid path"))
    ));

    let mut out_path = path.clone();
    out_path.set_extension("out");

    fs::remove_file(out_path).expect(error_fmt!(
      "(IOError) Failed to remove file: `{}`",
      path.to_str().expect(error_fmt!("(ArgError) Invalid path"))
    ));
  });
}

pub(super) fn evaluate_multi_input_test(files: &[String], out_file_name: &str) {
  let files = read_file_list(files, "vm");
  let programs = parse_programs(&files, VmParser::parse);
  let mut out_file = files.first().unwrap().clone();
  out_file.path.set_file_name(out_file_name);
  out_file.path.set_extension("asm");
  out_file.name = out_file.path.to_str().unwrap().to_string();

  let file_info = FileInfo::new(out_file.name, out_file.path, String::new());
  let mut program = Program::new(&file_info, Vec::new());
  program.fold_into(&programs);

  let output: Vec<(PathBuf, String)> =
    generate_output(&[program], &[file_info.clone()], None, "asm");

  output.iter().for_each(|(path, _)| {
    let mut tst_path = path.clone();
    tst_path.set_extension("tst");
    run_cpu_emulator(tst_path.to_str().unwrap());

    fs::remove_file(path).expect(error_fmt!(
      "(IOError) Failed to remove file: `{}`",
      path.to_str().expect(error_fmt!("(ArgError) Invalid path"))
    ));

    let mut out_path = path.clone();
    out_path.set_extension("out");

    fs::remove_file(out_path).expect(error_fmt!(
      "(IOError) Failed to remove file: `{}`",
      path.to_str().expect(error_fmt!("(ArgError) Invalid path"))
    ));
  });
}
