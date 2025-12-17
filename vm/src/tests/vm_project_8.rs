////////////////////////////////////////////////////////////////////////////////
// File: src/tests/vm_project_8.rs
// Description: VM translator tests for project 8
//
// Author: Leon Heidelbach <leon.heidelbach@hhu.de>
// Date: 06.05.2024
//
// License: GPLv3
////////////////////////////////////////////////////////////////////////////////

use super::util::{
  evaluate_multi_input_test, evaluate_single_input_test, VM_FILES_P8,
};

////////////////////////////////////////////////////////////////////////////////
// Test Cases Project 8
////////////////////////////////////////////////////////////////////////////////

#[test]
#[doc = "Test the VM translator's output using the nand2tetris CPU emulator."]
fn basic_loop() {
  evaluate_single_input_test(&[
    VM_FILES_P8.to_string() + "BasicLoop/BasicLoop.vm"
  ]);
}

#[test]
#[doc = "Test the VM translator's output using the nand2tetris CPU emulator."]
fn fibonacci_element() {
  evaluate_multi_input_test(
    &[
      VM_FILES_P8.to_string() + "FibonacciElement/Sys.vm",
      VM_FILES_P8.to_string() + "FibonacciElement/Main.vm",
    ],
    "FibonacciElement",
  );
}

#[test]
#[doc = "Test the VM translator's output using the nand2tetris CPU emulator."]
fn fibonacci_series() {
  evaluate_single_input_test(&[
    VM_FILES_P8.to_string() + "FibonacciSeries/FibonacciSeries.vm"
  ]);
}

#[test]
#[doc = "Test the VM translator's output using the nand2tetris CPU emulator."]
fn nested_call() {
  evaluate_multi_input_test(
    &[VM_FILES_P8.to_string() + "NestedCall/Sys.vm"],
    "NestedCall",
  );
}

#[test]
#[doc = "Test the VM translator's output using the nand2tetris CPU emulator."]
fn simple_function() {
  evaluate_single_input_test(&[
    VM_FILES_P8.to_string() + "SimpleFunction/SimpleFunction.vm"
  ]);
}

#[test]
#[doc = "Test the VM translator's output using the nand2tetris CPU emulator."]
fn statics_test() {
  evaluate_multi_input_test(
    &[
      VM_FILES_P8.to_string() + "StaticsTest/Sys.vm",
      VM_FILES_P8.to_string() + "StaticsTest/Class1.vm",
      VM_FILES_P8.to_string() + "StaticsTest/Class2.vm",
    ],
    "StaticsTest",
  );
}
