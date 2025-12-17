////////////////////////////////////////////////////////////////////////////////
// File: src/internal/index.rs
// Description: Indexer module
//
// Author: Leon Heidelbach <leon.heidelbach@hhu.de>
// Date: 15.05.2024
//
// License: GPLv3
////////////////////////////////////////////////////////////////////////////////

use std::{
  cell::{Cell, RefCell},
  collections::HashMap,
};

use super::{
  ast::{Identifier, ScopeInfo},
  check::Variable,
  jack::SymbolKind,
};

////////////////////////////////////////////////////////////////////////////////
// Indexer
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub(crate) struct Indexer {
  class_var_map: RefCell<HashMap<String, (u16, Variable)>>,
  var_map: RefCell<HashMap<String, (u16, Variable)>>,
  label_counter: Cell<u16>,
  static_var_counter: Cell<u16>,
  field_var_counter: Cell<u16>,
  arg_var_counter: Cell<u16>,
  local_var_counter: Cell<u16>,
}

impl Default for Indexer {
  fn default() -> Self {
    return Self {
      class_var_map: RefCell::new(HashMap::new()),
      var_map: RefCell::new(HashMap::new()),
      label_counter: Cell::new(0),
      static_var_counter: Cell::new(0),
      field_var_counter: Cell::new(0),
      arg_var_counter: Cell::new(0),
      local_var_counter: Cell::new(0),
    };
  }
}

impl Indexer {
  pub(crate) fn insert_var(
    &self,
    scope: &ScopeInfo,
    name: &str,
    var: Variable,
  ) {
    let map;
    let idx;

    match var.kind {
      SymbolKind::Static => {
        idx = self.static_var_counter.get();
        self.static_var_counter.replace(idx + 1);
        map = &self.class_var_map;
      }
      SymbolKind::Field => {
        idx = self.field_var_counter.get();
        self.field_var_counter.replace(idx + 1);
        map = &self.class_var_map;
      }
      SymbolKind::Argument => {
        idx = self.arg_var_counter.get();
        self.arg_var_counter.replace(idx + 1);
        map = &self.var_map;
      }
      SymbolKind::Local => {
        idx = self.local_var_counter.get();
        self.local_var_counter.replace(idx + 1);
        map = &self.var_map;
      }
    }

    map
      .borrow_mut()
      .insert(format!("{} > {}", scope.get_base_scope(), name), (idx, var));
  }

  pub(crate) fn get_var(
    &self,
    scope: &ScopeInfo,
    ident: &Identifier,
    known_class_var: bool,
  ) -> Option<(u16, Variable)> {
    let class_scope = format!("{} > {}", scope.class_name, ident.name);

    if known_class_var {
      return self.class_var_map.borrow().get(&class_scope).cloned();
    }

    let function_scope = format!("{} > {}", scope.get_base_scope(), ident.name);

    return match self.var_map.borrow().get(&function_scope) {
      Some(var) => Some(var.clone()),
      None => self.class_var_map.borrow().get(&class_scope).cloned(),
    };
  }

  pub(crate) fn reset_subroutine_counters(&self) {
    self.arg_var_counter.set(0);
    self.local_var_counter.set(0);
  }

  pub(crate) fn reset_class_counters(&self) {
    self.static_var_counter.set(0);
    self.field_var_counter.set(0);
  }

  pub(crate) fn new_label(&self) -> String {
    let current = self.label_counter.get();
    return format!("L{}", self.label_counter.replace(current + 1));
  }
}
