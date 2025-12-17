////////////////////////////////////////////////////////////////////////////////
// File: src/io/cli.rs
// Description: CLI parser
//
// Author: Leon Heidelbach <leon.heidelbach@hhu.de>
// Date: 01.05.2024
//
// License: GPLv3
////////////////////////////////////////////////////////////////////////////////

use std::env;

use crate::{error_fmt, error_panic, util::settings::Setting};

////////////////////////////////////////////////////////////////////////////////
// Commandline Parser Definitions
////////////////////////////////////////////////////////////////////////////////

pub trait DefaultArgType {
  fn default(&self) -> (u64, bool);
}

#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq)]
pub enum ArgType {
  Compile,
  Version([&'static str; 5]),
  Help([&'static str; 5]),
  Debug,
  Output,
  Xml,
  Liveness,
}

impl DefaultArgType for ArgType {
  fn default(&self) -> (u64, bool) {
    return match self {
      ArgType::Debug => (Setting::PrintAll as u64, true),
      ArgType::Output => (Setting::Output as u64, true),
      ArgType::Xml => (Setting::Xml as u64, true),
      ArgType::Liveness => (Setting::Liveness as u64, true),
      _ => (0, false),
    };
  }
}

#[derive(Debug, Copy, Clone, Ord, Eq, PartialOrd, PartialEq)]
pub enum ParseRuleType {
  Setting,
  Executable,
}

pub struct ParseRule {
  pub flag: char,
  pub long_flag: String,
  pub usage: String,
  pub description: String,
  pub arg_type: ArgType,
  pub parameters_min: usize,
  pub parameters_max: usize,
  pub rule_type: ParseRuleType,
  pub callback: fn(&mut CommandLineParser, &mut Token),
}

#[derive(Clone, Debug)]
pub struct Token {
  pub parse_rule_type: ParseRuleType,
  pub arg_type: ArgType,
  pub parameters: Option<Vec<String>>,
}

impl Token {
  pub fn new(
    parse_rule_type: ParseRuleType,
    arg_type: ArgType,
    parameters: Option<Vec<String>>,
  ) -> Self {
    return Self {
      parse_rule_type,
      arg_type,
      parameters,
    };
  }
}

////////////////////////////////////////////////////////////////////////////////
// Commandline Parser Implementation
////////////////////////////////////////////////////////////////////////////////

pub struct CommandLineParser {
  pub default_parse_rule: Option<usize>,
  pub tokens: Vec<Token>,
  pub parse_rules: Vec<ParseRule>,
}

impl Default for CommandLineParser {
  fn default() -> Self {
    return Self {
      parse_rules: Vec::new(),
      default_parse_rule: None,
      tokens: Vec::new(),
    };
  }
}

impl CommandLineParser {
  const FLAG_PREFIX: char = '-';
  const LONG_FLAG_PREFIX: &'static str = "--";

  pub fn default_parse_rule(
    &mut self,
    flag: char,
    long_flag: &str,
    usage: &str,
    description: &str,
    arg_type: ArgType,
    parameters_min: usize,
    parameters_max: usize,
    callback: fn(&mut CommandLineParser, &mut Token),
  ) -> &mut Self {
    assert!(parameters_min <= parameters_max);

    self.create_parse_rule(
      flag,
      long_flag,
      usage,
      description,
      arg_type,
      parameters_min,
      parameters_max,
      ParseRuleType::Executable,
      callback,
    );

    self.default_parse_rule = Some(self.parse_rules.len() - 1);

    return self;
  }

  pub fn create_parse_rule(
    &mut self,
    flag: char,
    long_flag: &str,
    usage: &str,
    description: &str,
    arg_type: ArgType,
    parameters_min: usize,
    parameters_max: usize,
    rule_type: ParseRuleType,
    callback: fn(&mut CommandLineParser, &mut Token),
  ) -> &mut Self {
    assert!(parameters_min <= parameters_max);

    self.parse_rules.push(ParseRule {
      flag,
      long_flag: long_flag.to_string(),
      usage: usage.to_string(),
      description: description.to_string(),
      arg_type,
      parameters_min,
      parameters_max,
      rule_type,
      callback,
    });

    return self;
  }

  fn find_parse_rules(&self, str: &str) -> Option<Vec<&ParseRule>> {
    let mut parse_rules = Vec::new();

    if str.starts_with(Self::LONG_FLAG_PREFIX) {
      let str = str.trim_start_matches(Self::LONG_FLAG_PREFIX);

      if let Some(parse_rule) = self
        .parse_rules
        .iter()
        .find(|rule| return rule.long_flag == str)
      {
        parse_rules.push(parse_rule);
        return Some(parse_rules);
      }
      error_panic!(
        "Invalid flag: `{}`. Use `--help` for more information.",
        str
      );
    } else if str.starts_with(Self::FLAG_PREFIX) {
      let str = str.trim_start_matches(Self::FLAG_PREFIX);

      for c in str.chars() {
        if let Some(parse_rule) =
          self.parse_rules.iter().find(|rule| return rule.flag == c)
        {
          parse_rules.push(parse_rule);
        } else {
          error_panic!(
            "Invalid flag: `{}`. Use `--help` for more information.",
            c
          );
        }
      }

      if !parse_rules.is_empty() {
        return Some(parse_rules);
      }
    }

    return None;
  }

  pub fn parse(&mut self) -> &mut Self {
    let mut args = env::args().peekable();
    let mut tokens = Vec::new();

    args.next().expect(error_fmt!("No app name"));

    while let Some(arg) = args.next() {
      if let Some(parse_rules) = self.find_parse_rules(&arg) {
        for parse_rule in parse_rules {
          let mut parameters: Vec<String> = Vec::new();

          while parameters.len() <= parse_rule.parameters_max {
            if let Some(parameter) = args.peek() {
              if parameter.starts_with(Self::FLAG_PREFIX) {
                break;
              }
              parameters.push(args.next().expect(error_fmt!("No parameter")));
            } else {
              break;
            }
          }

          if parameters.len() < parse_rule.parameters_min {
            error_panic!(
              "Not enough parameters for flag: `{}`. Use `--help` for more information.",
              arg
            );
          } else if parameters.len() > parse_rule.parameters_max {
            error_panic!(
              "Too many parameters for flag: `{}`. Use `--help` for more information.",
              arg
            );
          }

          tokens.push(Token::new(
            parse_rule.rule_type,
            parse_rule.arg_type,
            Some(parameters),
          ));
        }
      };
    }

    self.tokens = tokens;

    return self;
  }

  pub fn evaluate(&mut self) {
    if self.tokens.is_empty() {
      if let Some(default_parse_rule) = self.default_parse_rule {
        (self.parse_rules[default_parse_rule].callback)(
          self,
          &mut Token::new(
            self.parse_rules[default_parse_rule].rule_type,
            self.parse_rules[default_parse_rule].arg_type,
            None,
          ),
        );
      }
    }

    self.tokens.sort_by_key(|a| return a.parse_rule_type);

    for token in &mut self.tokens.clone() {
      let token_type = token.arg_type;
      if let Some(rule) = self
        .parse_rules
        .iter()
        .find(|rule| return rule.arg_type == token_type)
      {
        (rule.callback)(self, token);
      }
    }
  }
}
