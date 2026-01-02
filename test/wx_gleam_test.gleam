//// Test suite for the wx_gleam library.
////
//// This module contains the test runner for the wx_gleam project using gleeunit.

import gleeunit
import wx_gleam

/// Entry point for running the test suite.
///
/// This function initializes and runs all tests defined in the test directory.
pub fn main() {
  gleeunit.main()
}

pub fn start_test() {
  let assert Ok(_) = wx_gleam.start()
}
