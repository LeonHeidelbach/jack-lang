////////////////////////////////////////////////////////////////////////////////
// File: src/util/tokenize.rs
// Description: Tokenization module
//
// Author: Leon Heidelbach <leon.heidelbach@hhu.de>
// Date: 01.05.2024
//
// License: GPLv3
////////////////////////////////////////////////////////////////////////////////

use crate::red;
use crate::{error_panic, error_println, io::os::FileInfo};

use super::traits::Serializable;

use anyhow::Result;

pub fn parse_programs<'a, F, P>(files: &'a [FileInfo], parse: F) -> Vec<P>
where
  F: Fn(&'a FileInfo) -> Result<P>,
  P: Serializable + std::fmt::Debug,
{
  let (parse_results, parse_errors): (Vec<_>, Vec<_>) =
    files.iter().map(parse).partition(Result::is_ok);

  if !parse_errors.is_empty() {
    parse_errors
      .into_iter()
      .map(Result::unwrap_err)
      .for_each(|error| {
        error_println!("{}", error);
      });
    error_panic!(
      "(ParseError) Parsing failed. {}!",
      red!("Aborting compilation", true, false)
    );
  }

  return parse_results
    .into_iter()
    .map(Result::unwrap) // Safe to unwrap here
    .collect::<Vec<_>>();
}
