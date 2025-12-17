////////////////////////////////////////////////////////////////////////////////
// File: src/tests/util.rs
// Description: Jack Compiler tests
//
// Author: Leon Heidelbach <leon.heidelbach@hhu.de>
// Date: 15.05.2024
//
// License: GPLv3
////////////////////////////////////////////////////////////////////////////////

use std::{fs, path::PathBuf, sync::Once};

use shared::{
  error_fmt,
  io::os::{generate_output, read_file_list, FileInfo},
  util::traits::Serializable,
};

////////////////////////////////////////////////////////////////////////////////
// Constants
////////////////////////////////////////////////////////////////////////////////

pub(super) static INIT: Once = Once::new();
pub(super) const COMPILED_FILES_DIR: &str = "out";
pub(super) const JACK_AST_TEST_FILES: &str = "src/tests/10/";
pub(super) const JACK_VM_TEST_FILES: &str = "src/tests/11/";
pub(super) const JACK_EXTENSION: &str = "jack";
pub(super) const XML_EXTENSION: &str = "xml";
pub(super) const VM_EXTENSION: &str = "vm";

////////////////////////////////////////////////////////////////////////////////
// Test Suite Initialization
////////////////////////////////////////////////////////////////////////////////

pub(super) fn test_suite_init() {
  INIT.call_once(|| {
    if let Ok(false) = PathBuf::from(COMPILED_FILES_DIR).try_exists() {
      fs::create_dir_all(COMPILED_FILES_DIR).expect(error_fmt!(
        "(IOError) Failed to create directory: `{}`",
        COMPILED_FILES_DIR
      ));
    }
  });
}

pub(super) fn evaluate_test_file<S, F>(
  files: &[String],
  comparison_files: &[String],
  generator: F,
  out_file_ext: &str,
) where
  S: Serializable<Output = String>,
  F: Fn(&[FileInfo]) -> Vec<S>,
{
  assert_eq!(files.len(), comparison_files.len());
  test_suite_init();

  let files = read_file_list(files, JACK_EXTENSION);
  let mut comparison_files = read_file_list(comparison_files, out_file_ext);
  let programs = generator(&files);
  let output_dir = PathBuf::from(COMPILED_FILES_DIR);
  let mut output: Vec<(PathBuf, String)> =
    generate_output(&programs, &files, Some(&output_dir), out_file_ext);

  output.sort_by(|(p1, _), (p2, _)| return p1.cmp(p2));
  comparison_files.sort_by(|f1, f2| return f1.path.cmp(&f2.path));

  output.iter().zip(comparison_files.iter()).for_each(
    |((path, compiler_out), comparison)| {
      assert_eq!(compiler_out, &comparison.content.replace("\r\n", "\n"));

      let _ = fs::remove_file(path.with_extension(XML_EXTENSION));
    },
  );
}
