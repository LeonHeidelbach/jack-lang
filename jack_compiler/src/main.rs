////////////////////////////////////////////////////////////////////////////////
// File: src/main.rs
// Description: Main entry point
//
// Author: Leon Heidelbach <leon.heidelbach@hhu.de>
// Date: 15.05.2024
//
// License: GPLv3
////////////////////////////////////////////////////////////////////////////////

#![deny(clippy::implicit_return)]
#![allow(clippy::needless_return)]
#![allow(clippy::expect_fun_call)]
#![allow(clippy::too_many_arguments)]
#![allow(clippy::single_match)]

use anyhow::Result;
use shared::io::cli::{ArgType, CommandLineParser, ParseRuleType};
mod internal;

#[cfg(test)]
mod tests;

////////////////////////////////////////////////////////////////////////////////
// Initialize Package Info
////////////////////////////////////////////////////////////////////////////////

static PACKAGE_INFO: [&str; 5] = [
  env!("CARGO_PKG_NAME"),
  env!("CARGO_PKG_VERSION"),
  env!("CARGO_PKG_AUTHORS"),
  env!("CARGO_PKG_LICENSE"),
  env!("CARGO_PKG_DESCRIPTION"),
];

////////////////////////////////////////////////////////////////////////////////
// Main Entry Point
////////////////////////////////////////////////////////////////////////////////

fn main() -> Result<()> {
  CommandLineParser::default()
    .default_parse_rule(
      'h',
      "help",
      "-",
      "Print help",
      ArgType::Help(PACKAGE_INFO),
      0,
      0,
      shared::io::info::print_help,
    )
    .create_parse_rule(
      'v',
      "version",
      "-",
      "Print version",
      ArgType::Version(PACKAGE_INFO),
      0,
      0,
      ParseRuleType::Executable,
      shared::io::info::print_version,
    )
    .create_parse_rule(
      'd',
      "debug",
      "[(all|program|types|c_flow)*]",
      "Debug print settings (default: all)",
      ArgType::Debug,
      0,
      usize::MAX,
      ParseRuleType::Setting,
      shared::util::settings::set_global_setting,
    )
    .create_parse_rule(
      'o',
      "output",
      "(<dir>|<file>.hack)",
      "Set the output directory or output file name for the program",
      ArgType::Output,
      1,
      1,
      ParseRuleType::Setting,
      shared::util::settings::set_global_setting,
    )
    .create_parse_rule(
      'x',
      "xml",
      "<dir>?",
      "Generate AST XML files. Optionally specify the output directory",
      ArgType::Xml,
      0,
      1,
      ParseRuleType::Setting,
      shared::util::settings::set_global_setting,
    )
    .create_parse_rule(
      'l',
      "liveness",
      "-",
      "Liveness analysis",
      ArgType::Liveness,
      0,
      0,
      ParseRuleType::Setting,
      shared::util::settings::set_global_setting,
    )
    .create_parse_rule(
      'c',
      "compile",
      "[(<dir>|<file>.jack)+]",
      "Compile the list of .jack files or all .jack files in the directory",
      ArgType::Compile,
      1,
      usize::MAX,
      ParseRuleType::Executable,
      internal::compile::compile,
    )
    .parse()
    .evaluate();

  return Ok(());
}
