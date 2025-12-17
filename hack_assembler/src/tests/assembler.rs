////////////////////////////////////////////////////////////////////////////////
// File: src/tests/assembler.rs
// Description: Assembler tests
//
// Author: Leon Heidelbach <leon.heidelbach@hhu.de>
// Date: 01.05.2024
//
// License: GPLv3
////////////////////////////////////////////////////////////////////////////////

use std::{fs, path::PathBuf, sync::Once};

use crate::internal::parse::AsmParser;

use shared::{
  error_fmt,
  io::os::{generate_output, read_file_list},
  util::parse::parse_programs,
};

////////////////////////////////////////////////////////////////////////////////
// Constants
////////////////////////////////////////////////////////////////////////////////

static INIT: Once = Once::new();
const COMPILED_FILES_DIR: &str = "out";
const ASSEMBELER_TEST_ASM_FILES: &str = "src/tests/asm/";
const ASSEMBELER_TEST_COMPARISON_HACK_FILES: &str = "src/tests/hack/";

////////////////////////////////////////////////////////////////////////////////
// Test Suite Initialization
////////////////////////////////////////////////////////////////////////////////

fn test_suite_init() {
  INIT.call_once(|| {
    if let Ok(false) = PathBuf::from(COMPILED_FILES_DIR).try_exists() {
      fs::create_dir_all(COMPILED_FILES_DIR).expect(error_fmt!(
        "(IOError) Failed to create directory: `{}`",
        COMPILED_FILES_DIR
      ));
    }
  });
}

fn evaluate_test_file(files: &[String], comparison_files: &[String]) {
  assert_eq!(files.len(), comparison_files.len());
  test_suite_init();

  let files = read_file_list(files, "asm");
  let comparison_files = read_file_list(comparison_files, "hack");
  let programm = parse_programs(&files, AsmParser::parse);
  let output_dir = PathBuf::from(COMPILED_FILES_DIR);
  let output: Vec<(PathBuf, String)> =
    generate_output(&programm, &files, Some(&output_dir), "hack");

  output.iter().zip(comparison_files.iter()).for_each(
    |((_, assembler_out), comparison)| {
      assert_eq!(assembler_out, &comparison.content);
    },
  );
}

////////////////////////////////////////////////////////////////////////////////
// Test Cases
////////////////////////////////////////////////////////////////////////////////

#[test]
#[doc = "Test the assembler's output against the nand2tetris assembler's output."]
fn add() {
  evaluate_test_file(
    &[ASSEMBELER_TEST_ASM_FILES.to_string() + "Add.asm"],
    &[ASSEMBELER_TEST_COMPARISON_HACK_FILES.to_string() + "Add_compare.hack"],
  );
}

#[test]
#[doc = "Test the assembler's output against the nand2tetris assembler's output."]
fn max() {
  evaluate_test_file(
    &[ASSEMBELER_TEST_ASM_FILES.to_string() + "Max.asm"],
    &[ASSEMBELER_TEST_COMPARISON_HACK_FILES.to_string() + "Max_compare.hack"],
  );
}

#[test]
#[doc = "Test the assembler's output against the nand2tetris assembler's output."]
fn max_l() {
  evaluate_test_file(
    &[ASSEMBELER_TEST_ASM_FILES.to_string() + "MaxL.asm"],
    &[ASSEMBELER_TEST_COMPARISON_HACK_FILES.to_string() + "MaxL_compare.hack"],
  );
}

#[test]
#[doc = "Test the assembler's output against the nand2tetris assembler's output."]
fn pong() {
  evaluate_test_file(
    &[ASSEMBELER_TEST_ASM_FILES.to_string() + "Pong.asm"],
    &[ASSEMBELER_TEST_COMPARISON_HACK_FILES.to_string() + "Pong_compare.hack"],
  );
}

#[test]
#[doc = "Test the assembler's output against the nand2tetris assembler's output."]
fn pong_l() {
  evaluate_test_file(
    &[ASSEMBELER_TEST_ASM_FILES.to_string() + "PongL.asm"],
    &[ASSEMBELER_TEST_COMPARISON_HACK_FILES.to_string() + "PongL_compare.hack"],
  );
}

#[test]
#[doc = "Test the assembler's output against the nand2tetris assembler's output."]
fn rect() {
  evaluate_test_file(
    &[ASSEMBELER_TEST_ASM_FILES.to_string() + "Rect.asm"],
    &[ASSEMBELER_TEST_COMPARISON_HACK_FILES.to_string() + "Rect_compare.hack"],
  );
}

#[test]
#[doc = "Test the assembler's output against the nand2tetris assembler's output."]
fn rect_l() {
  evaluate_test_file(
    &[ASSEMBELER_TEST_ASM_FILES.to_string() + "RectL.asm"],
    &[ASSEMBELER_TEST_COMPARISON_HACK_FILES.to_string() + "RectL_compare.hack"],
  );
}
