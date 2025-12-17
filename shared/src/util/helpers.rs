////////////////////////////////////////////////////////////////////////////////
// File: src/util/helpers.rs
// Description: Helper functions
//
// Author: Leon Heidelbach <leon.heidelbach@hhu.de>
// Date: 01.05.2024
//
// License: GPLv3
////////////////////////////////////////////////////////////////////////////////

use std::iter::repeat;

use crate::error_fmt_src;

////////////////////////////////////////////////////////////////////////////////
// U16 Manipulation
////////////////////////////////////////////////////////////////////////////////

pub trait U16Manipulation {
  fn to_binary_string(&self) -> String;
  fn get_value_and_increment(&mut self) -> u16;
}

impl U16Manipulation for u16 {
  fn to_binary_string(&self) -> String {
    return format!("{:016b}", self);
  }

  fn get_value_and_increment(&mut self) -> u16 {
    let current = *self;
    *self = self
      .checked_add(1)
      .expect(error_fmt_src!("u16 overflow while incrementing"));
    return current;
  }
}

////////////////////////////////////////////////////////////////////////////////
// String Manipulation
////////////////////////////////////////////////////////////////////////////////

pub trait StrManipulation {
  fn remove_whitespaces(&self) -> String;
  fn indent(&self, indent: usize) -> String;
  fn pad_sides_with(
    &self,
    lpad: &str,
    rpad: &str,
    total_length: usize,
  ) -> String;
}

impl StrManipulation for str {
  fn remove_whitespaces(&self) -> String {
    return self
      .chars()
      .filter(|&c| return !c.is_whitespace())
      .collect();
  }

  fn indent(&self, indent: usize) -> String {
    return self
      .split('\n')
      .map(|line| {
        let mut indented_line = String::new();
        indented_line.extend(repeat('\t').take(indent));
        indented_line.push_str(line);
        return indented_line;
      })
      .collect::<Vec<String>>()
      .join("\n");
  }

  fn pad_sides_with(
    &self,
    lpad: &str,
    rpad: &str,
    total_length: usize,
  ) -> String {
    let mut padded_string = String::new();
    let side_length = (total_length - self.len()) / 2;
    if total_length > self.len() + 2 * side_length {
      padded_string.push_str(lpad);
    }
    padded_string.extend(repeat(lpad).take(side_length));
    padded_string.push_str(self);
    padded_string.extend(repeat(rpad).take(side_length));
    return padded_string;
  }
}
////////////////////////////////////////////////////////////////////////////////
// String Searching
////////////////////////////////////////////////////////////////////////////////

pub trait FindIgnoringWhitespace<T> {
  fn find_ignoring_whitespaces(&self, needle: T) -> Option<usize>;
}

impl FindIgnoringWhitespace<&str> for str {
  fn find_ignoring_whitespaces(&self, needle: &str) -> Option<usize> {
    let haystack_no_whitespace: String = self
      .chars()
      .filter(|&c| return !c.is_whitespace())
      .collect();
    let needle_no_whitespace: String = needle
      .chars()
      .filter(|&c| return !c.is_whitespace())
      .collect();

    if let Some(index) = haystack_no_whitespace.find(&needle_no_whitespace) {
      let whitespaces_before = self
        .chars()
        .take(index)
        .filter(|&c| return c.is_whitespace())
        .count();
      let adjusted_index = index + whitespaces_before;

      return Some(adjusted_index);
    } else {
      return None;
    }
  }
}

impl FindIgnoringWhitespace<char> for str {
  fn find_ignoring_whitespaces(&self, needle: char) -> Option<usize> {
    let needle_string = needle.to_string();
    return self.find_ignoring_whitespaces(needle_string.as_str());
  }
}
