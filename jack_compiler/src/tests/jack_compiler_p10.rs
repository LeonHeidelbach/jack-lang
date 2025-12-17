////////////////////////////////////////////////////////////////////////////////
// File: src/tests/jack_compiler_p10.rs
// Description: Jack Compiler tests
//
// Author: Leon Heidelbach <leon.heidelbach@hhu.de>
// Date: 15.05.2024
//
// License: GPLv3
////////////////////////////////////////////////////////////////////////////////

use crate::{
  internal::{
    parse::JackParser,
    xml::{OwnedXmlSerializableTokenStream, XmlSerializableProgram},
  },
  tests::util::XML_EXTENSION,
};

use super::util::{evaluate_test_file, JACK_AST_TEST_FILES};

use shared::{io::os::FileInfo, util::parse::parse_programs};

////////////////////////////////////////////////////////////////////////////////
// XML File Compilation
////////////////////////////////////////////////////////////////////////////////

fn parse_tokens(files: &[FileInfo]) -> Vec<OwnedXmlSerializableTokenStream> {
  return parse_programs(files, JackParser::parse)
    .into_iter()
    .map(OwnedXmlSerializableTokenStream::from)
    .collect();
}

fn parse_ast(files: &[FileInfo]) -> Vec<XmlSerializableProgram> {
  return parse_programs(files, JackParser::parse)
    .into_iter()
    .map(XmlSerializableProgram::from)
    .collect();
}

////////////////////////////////////////////////////////////////////////////////
// Test Cases: XML Serialization
////////////////////////////////////////////////////////////////////////////////

#[test]
#[doc = "Test the compiler's AST XML Serialization against the nand2tetris xml output files."]
fn array_token_test() {
  const REL_PATH: &str = "ArrayTest/";
  evaluate_test_file(
    &[JACK_AST_TEST_FILES.to_string() + REL_PATH + "Main.jack"],
    &[JACK_AST_TEST_FILES.to_string() + REL_PATH + "MainT.xml"],
    parse_tokens,
    XML_EXTENSION,
  );
}

#[test]
#[doc = "Test the compiler's AST XML Serialization against the nand2tetris xml output files."]
fn expression_less_square_main_token_test() {
  const REL_PATH: &str = "ExpressionLessSquare/";
  evaluate_test_file(
    &[JACK_AST_TEST_FILES.to_string() + REL_PATH + "Main.jack"],
    &[JACK_AST_TEST_FILES.to_string() + REL_PATH + "MainT.xml"],
    parse_tokens,
    XML_EXTENSION,
  );
}

#[test]
#[doc = "Test the compiler's AST XML Serialization against the nand2tetris xml output files."]
fn expression_less_square_square_token_test() {
  const REL_PATH: &str = "ExpressionLessSquare/";
  evaluate_test_file(
    &[JACK_AST_TEST_FILES.to_string() + REL_PATH + "Square.jack"],
    &[JACK_AST_TEST_FILES.to_string() + REL_PATH + "SquareT.xml"],
    parse_tokens,
    XML_EXTENSION,
  );
}

#[test]
#[doc = "Test the compiler's AST XML Serialization against the nand2tetris xml output files."]
fn expression_less_square_game_token_test() {
  const REL_PATH: &str = "ExpressionLessSquare/";
  evaluate_test_file(
    &[JACK_AST_TEST_FILES.to_string() + REL_PATH + "SquareGame.jack"],
    &[JACK_AST_TEST_FILES.to_string() + REL_PATH + "SquareGameT.xml"],
    parse_tokens,
    XML_EXTENSION,
  );
}

#[test]
#[doc = "Test the compiler's AST XML Serialization against the nand2tetris xml output files."]
fn square_main_token_test() {
  const REL_PATH: &str = "Square/";
  evaluate_test_file(
    &[JACK_AST_TEST_FILES.to_string() + REL_PATH + "Main.jack"],
    &[JACK_AST_TEST_FILES.to_string() + REL_PATH + "MainT.xml"],
    parse_tokens,
    XML_EXTENSION,
  );
}

#[test]
#[doc = "Test the compiler's AST XML Serialization against the nand2tetris xml output files."]
fn square_square_token_test() {
  const REL_PATH: &str = "Square/";
  evaluate_test_file(
    &[JACK_AST_TEST_FILES.to_string() + REL_PATH + "Square.jack"],
    &[JACK_AST_TEST_FILES.to_string() + REL_PATH + "SquareT.xml"],
    parse_tokens,
    XML_EXTENSION,
  );
}

#[test]
#[doc = "Test the compiler's AST XML Serialization against the nand2tetris xml output files."]
fn square_square_game_token_test() {
  const REL_PATH: &str = "Square/";
  evaluate_test_file(
    &[JACK_AST_TEST_FILES.to_string() + REL_PATH + "SquareGame.jack"],
    &[JACK_AST_TEST_FILES.to_string() + REL_PATH + "SquareGameT.xml"],
    |files| {
      return parse_programs(files, JackParser::parse)
        .into_iter()
        .map(OwnedXmlSerializableTokenStream::from)
        .collect();
    },
    XML_EXTENSION,
  );
}

////////////////////////////////////////////////////////////////////////////////
// Test Cases: XML AST Serialization
////////////////////////////////////////////////////////////////////////////////

#[test]
#[doc = "Test the compiler's AST XML Serialization against the nand2tetris xml output files."]
fn array_ast_test() {
  const REL_PATH: &str = "ArrayTest/";
  evaluate_test_file(
    &[JACK_AST_TEST_FILES.to_string() + REL_PATH + "Main.jack"],
    &[JACK_AST_TEST_FILES.to_string() + REL_PATH + "Main.xml"],
    parse_ast,
    XML_EXTENSION,
  );
}

#[test]
#[doc = "Test the compiler's AST XML Serialization against the nand2tetris xml output files."]
fn expression_less_square_main_ast_test() {
  const REL_PATH: &str = "ExpressionLessSquare/";
  evaluate_test_file(
    &[JACK_AST_TEST_FILES.to_string() + REL_PATH + "Main.jack"],
    &[JACK_AST_TEST_FILES.to_string() + REL_PATH + "Main.xml"],
    parse_ast,
    XML_EXTENSION,
  );
}

#[test]
#[doc = "Test the compiler's AST XML Serialization against the nand2tetris xml output files."]
fn expression_less_square_square_ast_test() {
  const REL_PATH: &str = "ExpressionLessSquare/";
  evaluate_test_file(
    &[JACK_AST_TEST_FILES.to_string() + REL_PATH + "Square.jack"],
    &[JACK_AST_TEST_FILES.to_string() + REL_PATH + "Square.xml"],
    parse_ast,
    XML_EXTENSION,
  );
}

#[test]
#[doc = "Test the compiler's AST XML Serialization against the nand2tetris xml output files."]
fn expression_less_square_game_ast_test() {
  const REL_PATH: &str = "ExpressionLessSquare/";
  evaluate_test_file(
    &[JACK_AST_TEST_FILES.to_string() + REL_PATH + "SquareGame.jack"],
    &[JACK_AST_TEST_FILES.to_string() + REL_PATH + "SquareGame.xml"],
    parse_ast,
    XML_EXTENSION,
  );
}

#[test]
#[doc = "Test the compiler's AST XML Serialization against the nand2tetris xml output files."]
fn square_main_ast_test() {
  const REL_PATH: &str = "Square/";
  evaluate_test_file(
    &[JACK_AST_TEST_FILES.to_string() + REL_PATH + "Main.jack"],
    &[JACK_AST_TEST_FILES.to_string() + REL_PATH + "Main.xml"],
    parse_ast,
    XML_EXTENSION,
  );
}

#[test]
#[doc = "Test the compiler's AST XML Serialization against the nand2tetris xml output files."]
fn square_square_ast_test() {
  const REL_PATH: &str = "Square/";
  evaluate_test_file(
    &[JACK_AST_TEST_FILES.to_string() + REL_PATH + "Square.jack"],
    &[JACK_AST_TEST_FILES.to_string() + REL_PATH + "Square.xml"],
    parse_ast,
    XML_EXTENSION,
  );
}

#[test]
#[doc = "Test the compiler's AST XML Serialization against the nand2tetris xml output files."]
fn square_square_game_ast_test() {
  const REL_PATH: &str = "Square/";
  evaluate_test_file(
    &[JACK_AST_TEST_FILES.to_string() + REL_PATH + "SquareGame.jack"],
    &[JACK_AST_TEST_FILES.to_string() + REL_PATH + "SquareGame.xml"],
    parse_ast,
    XML_EXTENSION,
  );
}
