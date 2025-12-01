//// Test suite for the example application.
////
//// This module contains tests for the example wx_gleam application using gleeunit.

import gleeunit

/// Entry point for running the example test suite.
///
/// This function initializes and runs all tests defined in the example test directory.
pub fn main() -> Nil {
  gleeunit.main()
}

/// A simple test to verify basic string concatenation.
///
/// This test demonstrates the gleeunit testing framework by creating a greeting
/// string and asserting it matches the expected value.
///
/// **Note:** gleeunit test functions must end in `_test` by convention.
pub fn hello_world_test() {
  let name = "Joe"
  let greeting = "Hello, " <> name <> "!"

  let assert "Hello, Joe!" = greeting
}
