////////////////////////////////////////////////////////////////////////////////
// File: src/tests/vm_project_7.rs
// Description: VM translator tests for project 7
//
// Author: Leon Heidelbach <leon.heidelbach@hhu.de>
// Date: 06.05.2024
//
// License: GPLv3
////////////////////////////////////////////////////////////////////////////////

use super::util::{evaluate_single_input_test, VM_FILES_P7};

////////////////////////////////////////////////////////////////////////////////
// Test Cases Project 7
////////////////////////////////////////////////////////////////////////////////

#[test]
#[doc = "Test the VM translator's output using the nand2tetris CPU emulator."]
fn basic_test() {
  evaluate_single_input_test(&[VM_FILES_P7.to_string() + "BasicTest.vm"]);
}

#[test]
#[doc = "Test the VM translator's output using the nand2tetris CPU emulator."]
fn pointer_test() {
  evaluate_single_input_test(&[VM_FILES_P7.to_string() + "PointerTest.vm"]);
}

#[test]
#[doc = "Test the VM translator's output using the nand2tetris CPU emulator."]
fn simple_add() {
  evaluate_single_input_test(&[VM_FILES_P7.to_string() + "SimpleAdd.vm"]);
}

#[test]
#[doc = "Test the VM translator's output using the nand2tetris CPU emulator."]
fn stack_test() {
  evaluate_single_input_test(&[VM_FILES_P7.to_string() + "StackTest.vm"]);
}

#[test]
#[doc = "Test the VM translator's output using the nand2tetris CPU emulator."]
fn static_test() {
  evaluate_single_input_test(&[VM_FILES_P7.to_string() + "StaticTest.vm"]);
}
