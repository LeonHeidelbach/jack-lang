////////////////////////////////////////////////////////////////////////////////
// File: src/settings.rs
// Description: Program settings
//
// Author: Leon Heidelbach <leon.heidelbach@hhu.de>
// Date: 01.05.2024
//
// License: GPLv3
////////////////////////////////////////////////////////////////////////////////

use std::{
  collections::{hash_map::Entry, HashMap},
  sync::RwLock,
};

use crate::{
  error_fmt_src, error_panic_src,
  io::cli::{ArgType, CommandLineParser, DefaultArgType, Token},
};

use anyhow::Result;

////////////////////////////////////////////////////////////////////////////////
// Global Settings
////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Debug)]
pub(crate) struct SettingContent {
  flag_val: u64,
  values: Option<Vec<String>>,
}

impl SettingContent {
  pub(crate) fn new(flag_val: u64, values: Option<Vec<String>>) -> Self {
    return Self { flag_val, values };
  }
}

impl Default for SettingContent {
  fn default() -> Self {
    return Self::new(0, None);
  }
}

lazy_static::lazy_static! {
  pub(crate) static ref GLOBAL_SETTINGS: RwLock<HashMap<ArgType, SettingContent>> = {
    let mut map = HashMap::new();
    map.insert(ArgType::Debug, SettingContent::default());
    map.insert(ArgType::Output, SettingContent::default());
    map.insert(ArgType::Xml, SettingContent::default());
    map.insert(ArgType::Liveness, SettingContent::default());
    return RwLock::new(map);
  };
}

pub enum Setting {
  PrintAll = 1 << 1_i64,
  PrintSymbols = 1 << 2_i64,
  PrintProgram = 1 << 3_i64,
  PrintTypes = 1 << 4_i64,
  PrintControlFlowGraph = 1 << 5_i64,
  Output = 1 << 6_i64,
  Xml = 1 << 7_i64,
  Liveness = 1 << 8_i64,
}

impl Setting {
  pub fn from_vec(v: Vec<String>, default: (u64, bool)) -> Result<(u64, bool)> {
    let (default_setting, set_value) = default;
    let mut value = 0;

    for s in v {
      match s.as_str() {
        "all" => {
          value |= Setting::PrintAll as u64;
        }
        "symbols" => {
          value |= Setting::PrintSymbols as u64;
        }
        "program" => {
          value |= Setting::PrintProgram as u64;
        }
        "types" => {
          value |= Setting::PrintTypes as u64;
        }
        "c_flow" => {
          value |= Setting::PrintControlFlowGraph as u64;
        }
        _ => {}
      }
    }

    if value == 0 {
      value = default_setting;
    }

    return Ok((value, set_value));
  }

  pub fn is_set(&self) -> Result<Option<Vec<String>>> {
    if let Ok(g) = GLOBAL_SETTINGS.read() {
      match self {
        Setting::PrintAll
        | Setting::PrintSymbols
        | Setting::PrintProgram
        | Setting::PrintTypes
        | Setting::PrintControlFlowGraph => {
          let debug_content =
            g.get(&ArgType::Debug).cloned().unwrap_or_default();

          match self {
            Setting::PrintAll => {
              if debug_content.flag_val & Setting::PrintAll as u64 != 0 {
                return Ok(None);
              }
            }
            Setting::PrintSymbols => {
              if debug_content.flag_val & Setting::PrintSymbols as u64 != 0 {
                return Ok(None);
              }
            }
            Setting::PrintProgram => {
              if debug_content.flag_val & Setting::PrintProgram as u64 != 0 {
                return Ok(None);
              }
            }
            Setting::PrintTypes => {
              if debug_content.flag_val & Setting::PrintTypes as u64 != 0 {
                return Ok(None);
              }
            }
            Setting::PrintControlFlowGraph => {
              if debug_content.flag_val & Setting::PrintControlFlowGraph as u64
                != 0
              {
                return Ok(None);
              }
            }
            _ => {}
          }
        }
        Setting::Output => {
          let output_content =
            g.get(&ArgType::Output).cloned().unwrap_or_default();

          if let Setting::Output = self {
            if output_content.flag_val & Setting::Output as u64 != 0 {
              return Ok(output_content.values);
            }
          }
        }
        Setting::Xml => {
          let xml_content = g.get(&ArgType::Xml).cloned().unwrap_or_default();

          if let Setting::Xml = self {
            if xml_content.flag_val & Setting::Xml as u64 != 0 {
              return Ok(xml_content.values);
            }
          }
        }
        Setting::Liveness => {
          let liveness_content =
            g.get(&ArgType::Liveness).cloned().unwrap_or_default();

          if let Setting::Liveness = self {
            if liveness_content.flag_val & Setting::Liveness as u64 != 0 {
              return Ok(None);
            }
          }
        }
      }
    }

    return Err(anyhow::anyhow!("(ArgError) Not set"));
  }
}

pub fn set_global_setting(_c: &mut CommandLineParser, t: &mut Token) {
  if let Ok(mut g) = GLOBAL_SETTINGS.write() {
    if let Entry::Occupied(mut e) = g.entry(t.arg_type) {
      let (flag, set_value) = Setting::from_vec(
        t.parameters.clone().unwrap_or_default(),
        t.arg_type.default(),
      )
      .expect(error_fmt_src!("(ArgError) Invalid setting"));

      if !set_value {
        e.insert(SettingContent::new(flag, None));
        return;
      }

      if let Some(p) = &t.parameters {
        e.insert(SettingContent::new(flag, Some(p.clone())));
      }

      return;
    }
  }

  error_panic_src!(
    "(ArgError) Could not set global setting key: {:?}",
    t.arg_type
  );
}
