////////////////////////////////////////////////////////////////////////////////
// File: src/tests/jack_compiler_p11.rs
// Description: Jack Compiler tests
//
// Author: Leon Heidelbach <leon.heidelbach@hhu.de>
// Date: 15.05.2024
//
// License: GPLv3
////////////////////////////////////////////////////////////////////////////////

use crate::{
  internal::{
    ast::Program,
    compile::typecheck_parse_trees,
    intermediate::{GeneratorInfo, IntermediateRepresentation},
    parse::JackParser,
  },
  tests::util::{JACK_VM_TEST_FILES, VM_EXTENSION},
};

use super::util::evaluate_test_file;

use shared::{io::os::FileInfo, util::parse::parse_programs};

////////////////////////////////////////////////////////////////////////////////
// Program Compilation
////////////////////////////////////////////////////////////////////////////////

fn parse_and_typecheck(files: &[FileInfo]) -> Vec<Program> {
  let mut programs = parse_programs(files, JackParser::parse);
  let type_map = typecheck_parse_trees(&programs, files);

  programs.iter_mut().enumerate().for_each(|(i, p)| {
    p.instructions
      .extend(p.intermediate(&GeneratorInfo::new(&files[i], &type_map)))
  });

  return programs;
}

////////////////////////////////////////////////////////////////////////////////
// Test Cases: Compilation
////////////////////////////////////////////////////////////////////////////////

#[test]
#[doc = "Test the compiler's generated VM files against previously tested output files."]
fn average_compile_test() {
  const REL_PATH: &str = "Average/";
  evaluate_test_file(
    &[JACK_VM_TEST_FILES.to_string() + REL_PATH],
    &[JACK_VM_TEST_FILES.to_string() + REL_PATH],
    parse_and_typecheck,
    VM_EXTENSION,
  );
}

#[test]
#[doc = "Test the compiler's generated VM files against previously tested output files."]
fn complex_array_compile_test() {
  const REL_PATH: &str = "ComplexArrays/";
  evaluate_test_file(
    &[JACK_VM_TEST_FILES.to_string() + REL_PATH],
    &[JACK_VM_TEST_FILES.to_string() + REL_PATH],
    parse_and_typecheck,
    VM_EXTENSION,
  );
}

#[test]
#[doc = "Test the compiler's generated VM files against previously tested output files."]
fn convert_to_bin_compile_test() {
  const REL_PATH: &str = "ConvertToBin/";
  evaluate_test_file(
    &[JACK_VM_TEST_FILES.to_string() + REL_PATH],
    &[JACK_VM_TEST_FILES.to_string() + REL_PATH],
    parse_and_typecheck,
    VM_EXTENSION,
  );
}

#[test]
#[doc = "Test the compiler's generated VM files against previously tested output files."]
fn pong_compile_test() {
  const REL_PATH: &str = "Pong/";
  evaluate_test_file(
    &[JACK_VM_TEST_FILES.to_string() + REL_PATH],
    &[JACK_VM_TEST_FILES.to_string() + REL_PATH],
    parse_and_typecheck,
    VM_EXTENSION,
  );
}

#[test]
#[doc = "Test the compiler's generated VM files against previously tested output files."]
fn seven_compile_test() {
  const REL_PATH: &str = "Seven/";
  evaluate_test_file(
    &[JACK_VM_TEST_FILES.to_string() + REL_PATH],
    &[JACK_VM_TEST_FILES.to_string() + REL_PATH],
    parse_and_typecheck,
    VM_EXTENSION,
  );
}

#[test]
#[doc = "Test the compiler's generated VM files against previously tested output files."]
fn square_compile_test() {
  const REL_PATH: &str = "Square/";
  evaluate_test_file(
    &[JACK_VM_TEST_FILES.to_string() + REL_PATH],
    &[JACK_VM_TEST_FILES.to_string() + REL_PATH],
    parse_and_typecheck,
    VM_EXTENSION,
  );
}
