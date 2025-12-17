////////////////////////////////////////////////////////////////////////////////
// File: src/io/info.rs
// Description: Program usage info
//
// Author: Leon Heidelbach <leon.heidelbach@hhu.de>
// Date: 01.05.2024
//
// License: GPLv3
////////////////////////////////////////////////////////////////////////////////

use crate::{cyan, error_unreachable, green, yellow};

use super::cli::{ArgType, CommandLineParser, Token};

////////////////////////////////////////////////////////////////////////////////
// General Usage Info & Debug Functions
////////////////////////////////////////////////////////////////////////////////

pub fn print_help(c: &mut CommandLineParser, t: &mut Token) {
  let flags = c
    .parse_rules
    .iter()
    .map(|parse_rule| {
      return format!(
        "\n  -{:1}    --{:9} {:29}   {}",
        parse_rule.flag,
        parse_rule.long_flag,
        parse_rule.usage,
        parse_rule.description
      );
    })
    .collect::<String>();
  let headings = green!(
    r#"
  Flag  Long Flag   Params                          Description
  ----  ---------   ------                          -----------"#,
    true,
    false
  );

  match t.arg_type {
    ArgType::Help(project_info) => {
      let [name, version, authors, ..] = project_info;

      println![
        r#"{} - {} v{}

{} {} {}
{}{}"#,
        authors,
        cyan!(name, true, false),
        version,
        green!("Usage:", true, false),
        cyan!(name, true, false),
        yellow!("[<Argument>*]", false, false),
        headings,
        flags
      ];
    }
    _ => {
      error_unreachable!(
        "Help token does not contain all necessary information"
      )
    }
  }
}

pub fn print_version(_c: &mut CommandLineParser, t: &mut Token) {
  match t.arg_type {
    ArgType::Version(project_info) => {
      let [name, version, authors, license, description] = project_info;
      println![
        "{}     {} v{}
{}      {} 2024
{}     {}
{} {}",
        green!("Version:", true, false),
        cyan!(name, true, false),
        version,
        green!("Author:", true, false),
        authors,
        green!("License:", true, false),
        license,
        green!("Description:", true, false),
        description
      ]
    }
    _ => {
      error_unreachable!(
        "Version token does not contain all necessary information"
      )
    }
  }
}
