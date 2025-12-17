////////////////////////////////////////////////////////////////////////////////
// File: src/internal/check.rs
// Description: Typechecking and indexing
//
// Author: Leon Heidelbach <leon.heidelbach@hhu.de>
// Date: 15.05.2024
//
// License: GPLv3
////////////////////////////////////////////////////////////////////////////////

use std::collections::HashMap;

use super::{
  ast::{
    Block, ClassDeclaration, ClassVariableDeclaration, Identifier, IfStatement,
    ReturnStatement, ScopeInfo, Statement, SubroutineCall, Type,
    VariableAssignment, VariableDeclaration, WhileStatement,
  },
  index::Indexer,
  intrinsics::INTRINSIC,
  jack::SymbolKind,
  tokenize::{Keyword, KEYWORD_THIS},
  types::{ExtractTypes, BUILTIN_TYPE_NULL},
};

use anyhow::Result;
use shared::{error_fmt, error_fmt_file, io::os::FileInfo, red};

////////////////////////////////////////////////////////////////////////////////
// Global Scope
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub(crate) struct GlobalScope {
  pub(crate) intrinsics: &'static HashMap<String, LocalScopeMap>,
  pub(crate) class_vars: HashMap<String, ClassVarMap>,
  pub(crate) map: HashMap<String, LocalScopeMap>,
  pub(crate) errors: Vec<String>,
  pub(crate) indexer: Indexer,
}

impl GlobalScope {
  pub(crate) fn new(
    intrinsics: &'static HashMap<String, LocalScopeMap>,
    class_vars: HashMap<String, ClassVarMap>,
    map: HashMap<String, LocalScopeMap>,
    errors: Vec<String>,
    indexer: Indexer,
  ) -> Self {
    return Self {
      map,
      intrinsics,
      class_vars,
      errors,
      indexer,
    };
  }

  pub(crate) fn factory() -> Self {
    return Self::new(
      &INTRINSIC,
      HashMap::new(),
      HashMap::new(),
      Vec::new(),
      Indexer::default(),
    );
  }

  pub(crate) fn get_base_scope(
    &mut self,
    scope: &ScopeInfo,
  ) -> Result<&mut LocalScopeMap> {
    if let Some(scope) = self.map.get_mut(&scope.get_base_scope()) {
      return Ok(scope);
    }

    return Err(anyhow::anyhow!(error_fmt!(
      "(TypeChecker) Could not find function scope `{}`.",
      scope
    )
    .to_owned()));
  }

  pub(crate) fn create_subscope(
    &mut self,
    inherited_scope: &ScopeInfo,
    scope: &ScopeInfo,
  ) {
    self.map.insert(
      scope.to_string(),
      self
        .map
        .get(&inherited_scope.to_string())
        .expect(error_fmt!(
          "(TypeChecker) Could not find {} scope `{}`.",
          scope.subroutine_type,
          scope
        ))
        .clone(),
    );
  }

  pub(crate) fn get_local_scope_map(
    &self,
    subroutine_name: &str,
    file_info: &FileInfo,
    pos: usize,
  ) -> &LocalScopeMap {
    return self
      .map
      .get(subroutine_name)
      .or_else(|| return self.intrinsics.get(subroutine_name))
      .expect(error_fmt_file!(
        &file_info.name,
        &file_info.content,
        pos,
        "(CodeGenerator)",
        "Subroutine `{}` not found in local scope.",
        subroutine_name
      ));
  }

  pub(crate) fn insert_into_local_scope_map(
    &mut self,
    key: &str,
    value: &Variable,
    scope: &ScopeInfo,
  ) -> Option<Variable> {
    return self
      .map
      .get_mut(&scope.to_string())
      .expect(error_fmt!(
        "(TypeChecker) Could not find {} scope `{}`.",
        scope.subroutine_type,
        scope
      ))
      .map
      .insert(key.to_string(), value.clone());
  }

  pub(crate) fn errors_as_result(&self) -> Result<&GlobalScope> {
    return if self.errors.is_empty() {
      Ok(self)
    } else {
      Err(anyhow::anyhow!(
        "{}\n{}",
        self.errors.join("\n"),
        error_fmt!(
          "Typechecking failed with {} error(s).",
          red!(&self.errors.len().to_string(), true, false)
        )
      ))
    };
  }

  fn get_class_var(
    &self,
    scope: &ScopeInfo,
    ident: &Identifier,
    file_info: &FileInfo,
  ) -> Result<Variable> {
    if let Some(result) = self.class_vars.get(&scope.class_name) {
      if let Some(result) = result.map.get(&ident.name) {
        if result.kind == SymbolKind::Field
          && scope.subroutine_type == Keyword::Function
        {
          return Err(anyhow::anyhow!(error_fmt_file!(
            &file_info.name,
            &file_info.content,
            ident.pos,
            "(TypeChecker)",
            "Variable `{}` is a class field and cannot be accessed \
              in static {} scope `{}` of class `{}`.",
            ident.name,
            scope.subroutine_type,
            scope,
            scope.class_name
          )
          .to_string()));
        } else {
          return Ok(result.clone());
        }
      }
    }

    return Err(anyhow::anyhow!(error_fmt_file!(
      &file_info.name,
      &file_info.content,
      ident.pos,
      "(TypeChecker)",
      "Variable `{}` is not a static or field variable in class `{}`.",
      ident.name,
      scope.class_name
    )
    .to_string()));
  }

  pub(crate) fn get_var_info(
    &self,
    scope: &ScopeInfo,
    ident: &Identifier,
    is_class_var: bool,
    file_info: &FileInfo,
  ) -> Result<Variable> {
    if is_class_var {
      return self.get_class_var(scope, ident, file_info);
    } else if let Some(result) = self
      .get_local_scope_map(&scope.to_string(), file_info, ident.pos)
      .map
      .get(&ident.name)
    {
      return Ok(result.clone());
    } else if let Ok(result) = self.get_class_var(scope, ident, file_info) {
      return Ok(result);
    }

    return Err(anyhow::anyhow!(error_fmt_file!(
      &file_info.name,
      &file_info.content,
      ident.pos,
      "(TypeChecker)",
      "Variable `{}` could not be found in {} scope `{}`.",
      ident.name,
      scope.subroutine_type,
      &scope.get_base_scope()
    )
    .to_string()));
  }
}

////////////////////////////////////////////////////////////////////////////////
// Local Function Scopes
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub(crate) struct LocalScopeMap {
  pub(crate) subroutine_info: SubroutineInfo,
  pub(crate) map: HashMap<String, Variable>,
}

impl LocalScopeMap {
  pub(crate) fn new(
    function_info: SubroutineInfo,
    local_scope: HashMap<String, Variable>,
  ) -> Self {
    return Self {
      subroutine_info: function_info,
      map: local_scope,
    };
  }

  pub(crate) fn factory(
    r#type: Keyword,
    return_type: Type,
    param_types: Vec<Type>,
    expected_returns: isize,
  ) -> Self {
    return Self::new(
      SubroutineInfo::new(r#type, return_type, param_types, expected_returns),
      HashMap::new(),
    );
  }
}

impl Default for LocalScopeMap {
  fn default() -> Self {
    return Self::new(
      SubroutineInfo::new(Keyword::Function, Type::Void, Vec::new(), 0),
      HashMap::new(),
    );
  }
}

////////////////////////////////////////////////////////////////////////////////
// Function Info
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub(crate) struct SubroutineInfo {
  pub(crate) r#type: Keyword,
  pub(crate) return_type: Type,
  pub(crate) param_types: Vec<Type>,
  pub(crate) expected_returns: isize,
}

impl SubroutineInfo {
  fn new(
    r#type: Keyword,
    return_type: Type,
    param_types: Vec<Type>,
    expected_returns: isize,
  ) -> Self {
    return Self {
      r#type,
      return_type,
      param_types,
      expected_returns,
    };
  }
}

////////////////////////////////////////////////////////////////////////////////
// Class Variables
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub(crate) struct ClassVarMap {
  pub(crate) map: HashMap<String, Variable>,
}

impl ClassVarMap {
  pub(crate) fn new(map: HashMap<String, Variable>) -> Self {
    return Self { map };
  }

  pub(crate) fn factory() -> Self {
    return Self::new(HashMap::new());
  }

  pub(crate) fn get_count_of_kind(&self, kind: SymbolKind) -> usize {
    return self
      .map
      .values()
      .filter(|variable| return variable.kind == kind)
      .count();
  }
}

#[derive(Debug, Clone)]
pub(crate) struct Variable {
  pub(crate) r#type: Type,
  pub(crate) kind: SymbolKind,
}

impl Variable {
  pub(crate) fn new(r#type: Type, kind: SymbolKind) -> Self {
    return Self { r#type, kind };
  }
}

////////////////////////////////////////////////////////////////////////////////
// Class Information
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
pub(crate) struct ClassInfo<'a> {
  pub(crate) file_info: &'a FileInfo,
  pub(crate) class_dec: &'a ClassDeclaration,
}

#[derive(Debug, Default)]
pub(crate) struct TypeCheckableProgram<'a> {
  pub(crate) classes: Vec<ClassInfo<'a>>,
}

impl<'a> TypeCheckableProgram<'a> {
  pub(crate) fn insert_class_declaration(
    &mut self,
    class_dec: &'a ClassDeclaration,
    file_info: &'a FileInfo,
  ) {
    self.classes.push(ClassInfo {
      file_info,
      class_dec,
    });
  }
}

////////////////////////////////////////////////////////////////////////////////
// TypeChecker
////////////////////////////////////////////////////////////////////////////////

pub(crate) struct TypeChecker<'a> {
  pub(crate) program: &'a TypeCheckableProgram<'a>,
  pub(crate) scopes: GlobalScope,
}

impl<'a> TypeChecker<'a> {
  pub(crate) fn new(
    program: &'a TypeCheckableProgram,
    scopes: GlobalScope,
  ) -> Self {
    return Self { program, scopes };
  }

  pub(crate) fn factory(program: &'a TypeCheckableProgram) -> Self {
    return Self::new(program, GlobalScope::factory());
  }

  //////////////////////////////////////////////////////////////////////////////
  // Typechecking
  //////////////////////////////////////////////////////////////////////////////

  pub(crate) fn typecheck(&mut self) -> Result<&GlobalScope> {
    self.create_class_index();

    for class_info in self.program.classes.iter() {
      self.scopes.indexer.reset_class_counters();

      for declaration in class_info.class_dec.class_vars.iter() {
        self.typecheck_class_variable_declaration(
          declaration,
          &class_info.class_dec.identifier.name,
          class_info.file_info,
        )?;
      }

      for function in class_info.class_dec.subroutines.iter() {
        self.scopes.indexer.reset_subroutine_counters();

        self.typecheck_block(&function.body, class_info.file_info)?;

        let scope = self.scopes.get_local_scope_map(
          &function.scope.to_string(),
          class_info.file_info,
          function.pos,
        );

        if scope.subroutine_info.expected_returns > 0 {
          self.scopes.errors.push(
            error_fmt_file!(
              &class_info.file_info.name,
              &class_info.file_info.content,
              function.pos,
              "(TypeChecker)",
              "Missing return statements in function scope `{}`. The \
              function expected to return `{}` on all control flow paths.",
              function.scope,
              scope.subroutine_info.return_type
            )
            .to_owned(),
          );
        }
      }
    }

    return self.scopes.errors_as_result();
  }

  //////////////////////////////////////////////////////////////////////////////
  // Indexing
  //////////////////////////////////////////////////////////////////////////////

  fn create_class_index(&mut self) {
    for class_info in self.program.classes.iter() {
      self.scopes.indexer.reset_class_counters();
      self.create_class_var_index(class_info);
      self.create_subroutine_index(class_info);
    }
  }

  fn create_class_var_index(&mut self, class_info: &ClassInfo) {
    let mut class_var_map = ClassVarMap::factory();

    for field in class_info.class_dec.class_vars.iter() {
      for ident in field.identifiers.iter() {
        let var =
          Variable::new(field.type_dec.raw_type.clone(), field.var_type);

        if let Some(variable) =
          class_var_map.map.insert(ident.name.clone(), var.clone())
        {
          self.scopes.errors.push(
            error_fmt_file!(
              &class_info.file_info.name,
              &class_info.file_info.content,
              ident.pos,
              "(TypeChecker) ",
              "Variable `{}` has already been declared in class `{}` \
                with type `{}`",
              ident.name,
              class_info.class_dec.identifier.name,
              variable.r#type
            )
            .to_owned(),
          );
        }

        self
          .scopes
          .indexer
          .insert_var(&ident.scope, &ident.name, var);
      }
    }

    if self
      .scopes
      .class_vars
      .insert(class_info.class_dec.identifier.name.clone(), class_var_map)
      .is_some()
    {
      self.scopes.errors.push(
        error_fmt_file!(
          &class_info.file_info.name,
          &class_info.file_info.content,
          class_info.class_dec.pos,
          "(TypeChecker) ",
          "Class `{}` has already been declared.",
          class_info.class_dec.identifier.name
        )
        .to_owned(),
      );
    }
  }

  fn create_subroutine_index(&mut self, class_info: &ClassInfo) {
    for subroutine in class_info.class_dec.subroutines.iter() {
      self.scopes.indexer.reset_subroutine_counters();

      match subroutine.subroutine_type {
        Keyword::Constructor => {
          if subroutine.return_type.raw_type
            != Type::Complex(class_info.class_dec.identifier.name.clone())
          {
            self.scopes.errors.push(
              error_fmt_file!(
                &class_info.file_info.name,
                &class_info.file_info.content,
                subroutine.pos,
                "(TypeChecker) ",
                "Constructor `{}` has invalid return type `{}`. \
                  Expected: `{}`.",
                subroutine.identifier.name,
                subroutine.return_type.raw_type,
                &class_info.class_dec.identifier.name
              )
              .to_owned(),
            );
          }
        }
        Keyword::Method => {
          let var = Variable::new(
            Type::Complex(class_info.class_dec.identifier.name.clone()),
            SymbolKind::Argument,
          );

          self
            .scopes
            .indexer
            .insert_var(&subroutine.scope, KEYWORD_THIS, var);
        }
        _ => {}
      }

      if let Some(sub) = self.scopes.map.insert(
        subroutine.scope.to_string(),
        LocalScopeMap::factory(
          subroutine.subroutine_type,
          subroutine.return_type.clone().raw_type,
          subroutine
            .params
            .iter()
            .map(|param| return param.type_dec.raw_type.clone())
            .collect(),
          if subroutine.return_type.raw_type != Type::Void {
            1
          } else {
            0
          },
        ),
      ) {
        self.scopes.errors.push(
          error_fmt_file!(
            &class_info.file_info.name,
            &class_info.file_info.content,
            subroutine.pos,
            "(TypeChecker) ",
            "Function `{}` has already been declared with return type `{}`.",
            subroutine.scope,
            sub.subroutine_info.return_type
          )
          .to_owned(),
        );
      }

      subroutine.params.iter().for_each(|param| {
        let var =
          Variable::new(param.type_dec.raw_type.clone(), SymbolKind::Argument);
        self.scopes.insert_into_local_scope_map(
          &param.identifier.name,
          &var,
          &subroutine.scope,
        );

        self.scopes.indexer.insert_var(
          &subroutine.scope,
          &param.identifier.name,
          var,
        );
      });
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Blocks
  //////////////////////////////////////////////////////////////////////////////

  fn typecheck_block(
    &mut self,
    block: &Block,
    file_info: &FileInfo,
  ) -> Result<()> {
    let statement_count = block.statements.len();

    for (i, statement) in block.statements.iter().enumerate() {
      match statement {
        Statement::VariableDeclaration(ref declaration) => {
          self.typecheck_variable_declaration(declaration, file_info)?
        }
        Statement::VariableAssignment(ref assignment) => {
          self.typecheck_variable_assignment(assignment, file_info)?
        }
        Statement::If(if_statement) => self.typecheck_if(
          if_statement,
          file_info,
          i == statement_count - 1,
        )?,
        Statement::While(while_statement) => {
          self.typecheck_while(while_statement, file_info)?
        }
        Statement::Return(return_statement) => {
          self.typecheck_return(return_statement, file_info)?
        }
        Statement::SubroutineCall(ref function_call) => {
          self.typecheck_function_call(function_call, file_info)?
        }
      }
    }

    return Ok(());
  }

  //////////////////////////////////////////////////////////////////////////////
  // Variable Declarations & Assignments
  //////////////////////////////////////////////////////////////////////////////

  fn typecheck_class_variable_declaration(
    &mut self,
    declaration: &ClassVariableDeclaration,
    class_name: &str,
    file_info: &FileInfo,
  ) -> Result<()> {
    for ident in declaration.identifiers.iter() {
      let r#type = &declaration.type_dec.raw_type;
      if r#type.is_complex()
        && !r#type.is_builtin()
        && !self.program.classes.iter().any(|class| {
          return class.class_dec.identifier.name
            == declaration.type_dec.raw_type.to_string();
        })
      {
        self.scopes.errors.push(
          error_fmt_file!(
            &file_info.name,
            &file_info.content,
            declaration.type_dec.pos,
            "(TypeChecker) ",
            "Type mismatch in class declaration `{}`. Class variable \
              `{}` has been declared with an unknown type `{}`.",
            class_name,
            ident.name,
            declaration.type_dec.raw_type
          )
          .to_owned(),
        );
      }
    }

    return Ok(());
  }

  fn typecheck_variable_declaration(
    &mut self,
    declaration: &VariableDeclaration,
    file_info: &FileInfo,
  ) -> Result<()> {
    for ident in declaration.identifiers.iter() {
      let r#type = &declaration.type_dec.raw_type;
      if r#type.is_complex()
        && !r#type.is_builtin()
        && !self.program.classes.iter().any(|class| {
          return class.class_dec.identifier.name
            == declaration.type_dec.raw_type.to_string();
        })
      {
        self.scopes.errors.push(
          error_fmt_file!(
            &file_info.name,
            &file_info.content,
            declaration.type_dec.pos,
            "(TypeChecker) ",
            "Type mismatch in function scope `{}`. Variable `{}` \
              has been declared with an unknown type `{}`.",
            declaration.scope,
            ident.name,
            declaration.type_dec.raw_type
          )
          .to_owned(),
        );
      }

      let var =
        Variable::new(declaration.type_dec.raw_type.clone(), SymbolKind::Local);
      if let Some(variable) = self.scopes.insert_into_local_scope_map(
        &ident.name,
        &var,
        &declaration.scope,
      ) {
        self.scopes.errors.push(
          error_fmt_file!(
            &file_info.name,
            &file_info.content,
            ident.pos,
            "(TypeChecker) ",
            "Type mismatch in function scope `{}`. Variable `{}` has \
              already been declared with type `{}`.",
            declaration.scope,
            ident.name,
            variable.r#type
          )
          .to_owned(),
        );
      }

      self
        .scopes
        .indexer
        .insert_var(&ident.scope, &ident.name, var);
    }

    return Ok(());
  }

  fn typecheck_variable_assignment(
    &mut self,
    assignment: &VariableAssignment,
    file_info: &FileInfo,
  ) -> Result<()> {
    let expr_types = assignment.value.types(&self.scopes, file_info);

    if let Err(error) = expr_types {
      self.scopes.errors.push(error.to_string());
      return Ok(());
    }

    match self.scopes.get_var_info(
      &assignment.scope,
      &assignment.identifier,
      assignment.is_class_var,
      file_info,
    ) {
      Ok(variable) => {
        let null_type = Type::Complex(String::from(BUILTIN_TYPE_NULL));
        let assignment_to_complex = variable.r#type.is_complex();

        for (expr, (expr_type, expr_pos)) in expr_types.unwrap() {
          let non_complex_null_assign =
            !assignment_to_complex && expr_type == null_type;

          if expr_type != variable.r#type
            && !expr_type
              .is_coerceable(&variable.r#type)
              .safe_unwrap(file_info, expr_pos)
          {
            self.scopes.errors.push(
              error_fmt_file!(
                &file_info.name,
                &file_info.content,
                expr_pos,
                "(TypeChecker)",
                "Type mismatch in function scope `{}`. Variable `{}` \
                  of type `{}` was assigned `{}` of type `{}`.{}",
                assignment.scope,
                assignment.identifier.name,
                variable.r#type,
                expr,
                expr_type,
                if non_complex_null_assign {
                  " Null can only be assigned to complex types."
                } else {
                  ""
                }
              )
              .to_owned(),
            );
          }
        }
      }
      Err(err) => {
        self.scopes.errors.push(err.to_string());
      }
    }
    return Ok(());
  }

  //////////////////////////////////////////////////////////////////////////////
  // Statements
  //////////////////////////////////////////////////////////////////////////////

  fn typecheck_if(
    &mut self,
    if_statement: &IfStatement,
    file_info: &FileInfo,
    is_last_statement: bool,
  ) -> Result<()> {
    let expr_types = if_statement.condition.types(&self.scopes, file_info);

    if let Err(error) = expr_types {
      self.scopes.errors.push(error.to_string());
      return Ok(());
    }

    for (expr, (expr_type, expr_pos)) in expr_types.unwrap() {
      if expr_type != Type::Boolean
        && !expr_type
          .is_coerceable(&Type::Boolean)
          .safe_unwrap(file_info, expr_pos)
      {
        self.scopes.errors.push(
          error_fmt_file!(
            &file_info.name,
            &file_info.content,
            expr_pos,
            "(TypeChecker)",
            "Type mismatch in {} scope `{}`. If statement did \
              not evaluate to a boolean expression and included \
              value of type `{}`. Found: {}.",
            if_statement.scope.subroutine_type,
            if_statement.scope,
            expr_type,
            expr
          )
          .to_owned(),
        );
      }
    }

    ////////////////////////////////////////////////////////////////////////////
    // Return Statement Counter Setup
    ////////////////////////////////////////////////////////////////////////////

    let scope = self.scopes.get_base_scope(&if_statement.scope)?;
    let return_statements = scope.subroutine_info.expected_returns;
    let is_void = scope.subroutine_info.return_type == Type::Void;
    let mut return_in_if = false;

    ////////////////////////////////////////////////////////////////////////////
    // Return Statement Count & Type Check If Block
    ////////////////////////////////////////////////////////////////////////////

    if !if_statement.if_body.statements.is_empty() {
      if !is_void {
        let scope = self.scopes.get_base_scope(&if_statement.scope)?;
        scope.subroutine_info.expected_returns += 1;
      }

      self
        .scopes
        .create_subscope(&if_statement.scope, &if_statement.scope_if);

      self.typecheck_block(&if_statement.if_body, file_info)?;

      if !is_void {
        let scope = self.scopes.get_base_scope(&if_statement.scope)?;
        if scope.subroutine_info.expected_returns > return_statements {
          scope.subroutine_info.expected_returns = return_statements;
        } else {
          return_in_if = true;
        }
      }
    }

    ////////////////////////////////////////////////////////////////////////////
    // Return Statement Count & Type Check Else Block
    ////////////////////////////////////////////////////////////////////////////

    if let Some(else_body) = &if_statement.else_body {
      if !is_void {
        let scope = self.scopes.get_base_scope(&if_statement.scope)?;
        scope.subroutine_info.expected_returns += 1;
      }

      self
        .scopes
        .create_subscope(&if_statement.scope, &if_statement.scope_else);

      self.typecheck_block(else_body, file_info)?;
    }

    ////////////////////////////////////////////////////////////////////////////
    // Return Statement Counter Assertion
    ////////////////////////////////////////////////////////////////////////////

    if !is_void {
      let scope = self.scopes.get_base_scope(&if_statement.scope)?;

      if return_in_if
        && scope.subroutine_info.expected_returns > return_statements
      {
        self.scopes.errors.push(
          error_fmt_file!(
            &file_info.name,
            &file_info.content,
            if_statement.pos,
            "(TypeChecker)",
            "Type mismatch in {} scope `{}`. If statement expected \
            to return on all control flow paths but did not.",
            if_statement.scope.subroutine_type,
            if_statement.scope
          )
          .to_owned(),
        );
      } else {
        scope.subroutine_info.expected_returns = if return_in_if
          && is_last_statement
          && if_statement.else_body.is_some()
        {
          return_statements - 1
        } else {
          return_statements
        };
      }
    }

    return Ok(());
  }

  fn typecheck_while(
    &mut self,
    while_statement: &WhileStatement,
    file_info: &FileInfo,
  ) -> Result<()> {
    let expr_types = while_statement.condition.types(&self.scopes, file_info);

    if let Err(error) = expr_types {
      self.scopes.errors.push(error.to_string());
      return Ok(());
    }

    for (expr, (expr_type, expr_pos)) in expr_types.unwrap() {
      if expr_type != Type::Boolean
        && !expr_type
          .is_coerceable(&Type::Boolean)
          .safe_unwrap(file_info, expr_pos)
      {
        self.scopes.errors.push(
          error_fmt_file!(
            &file_info.name,
            &file_info.content,
            expr_pos,
            "(TypeChecker)",
            "Type mismatch in {} scope {}. While statement did not \
            evaluate to a boolean expression and included value of \
            type `{}`. Found: {}.",
            while_statement.scope.subroutine_type,
            while_statement.scope,
            expr_type,
            expr
          )
          .to_owned(),
        );
      }
    }

    self
      .scopes
      .create_subscope(&while_statement.scope, &while_statement.scope_while);

    self.typecheck_block(&while_statement.body, file_info)?;
    return Ok(());
  }

  fn typecheck_return(
    &mut self,
    return_statement: &ReturnStatement,
    file_info: &FileInfo,
  ) -> Result<()> {
    let expr_types = return_statement
      .value
      .as_ref()
      .map(|expr| {
        return expr.types(&self.scopes, file_info);
      })
      .unwrap_or_else(|| {
        return Ok(vec![(
          "Void".to_string(),
          (Type::Void, return_statement.pos),
        )]);
      });

    if let Err(error) = expr_types {
      self.scopes.errors.push(error.to_string());
      return Ok(());
    }

    if let Ok(scope) = self.scopes.get_base_scope(&return_statement.scope) {
      let return_type = scope.subroutine_info.return_type.clone();
      scope.subroutine_info.expected_returns -= 1;

      for (expr, (expr_type, expr_pos)) in expr_types.unwrap() {
        if expr_type != return_type
          && !expr_type
            .is_coerceable(&return_type)
            .safe_unwrap(file_info, expr_pos)
        {
          self.scopes.errors.push(
            error_fmt_file!(
              &file_info.name,
              &file_info.content,
              expr_pos,
              "(TypeChecker)",
              "Type mismatch in {} scope `{}`. Return statement expected \
              return type `{}` but got `{}`. Found: {}.",
              return_statement.scope.subroutine_type,
              return_statement.scope,
              return_type,
              expr_type,
              expr
            )
            .to_owned(),
          );
        }
      }
    } else {
      self.scopes.errors.push(
        error_fmt!(
          "(TypeChecker) Could not find {} scope `{}` while evaluating \
          return statement.",
          return_statement.scope.subroutine_type,
          return_statement.scope
        )
        .to_owned(),
      );
    }

    return Ok(());
  }

  fn typecheck_function_call(
    &mut self,
    subroutine_call: &SubroutineCall,
    file_info: &FileInfo,
  ) -> Result<()> {
    let class_name = if let Ok(variable) = &self.scopes.get_var_info(
      &subroutine_call.scope,
      &subroutine_call.class_var_ref,
      subroutine_call.has_this_class_specifier,
      file_info,
    ) {
      variable.r#type.to_string()
    } else {
      subroutine_call.class_var_ref.name.clone()
    };
    let subroutine_name =
      format!("{}.{}", class_name, subroutine_call.identifier.name);
    let is_intrinsics = self.scopes.intrinsics.contains_key(&subroutine_name);
    let mut search_map = &self.scopes.map;

    if is_intrinsics {
      search_map = self.scopes.intrinsics;
    }

    let subroutine = search_map.get(&subroutine_name);

    if let Some(subroutine) = subroutine {
      let subroutine_info = &subroutine.subroutine_info;
      for (i, param) in subroutine_call.params.iter().enumerate() {
        let expr_types = param.types(&self.scopes, file_info);

        if expr_types.is_err() {
          self.scopes.errors.push(
            error_fmt!(
              "(TypeChecker) Type mismatch in {} scope `{}`. Call \
                `{}` included errors:\n{}.",
              subroutine_call.scope.subroutine_type,
              subroutine_call.scope,
              subroutine_name,
              expr_types.unwrap_err()
            )
            .to_owned(),
          );
          return Ok(());
        }

        for (expr, (expr_type, expr_pos)) in expr_types.unwrap() {
          if let Some(param_type) = subroutine_info.param_types.get(i) {
            if expr_type != *param_type
              && !expr_type
                .is_coerceable(param_type)
                .safe_unwrap(file_info, expr_pos)
            {
              self.scopes.errors.push(
                error_fmt_file!(
                  &file_info.name,
                  &file_info.content,
                  expr_pos,
                  "(TypeChecker)",
                  "Type mismatch in {} scope `{}`. Call `{}` \
                    expected parameter of type `{}` at index {} but got `{}`. \
                    Found: {}.",
                  subroutine_call.scope.subroutine_type,
                  subroutine_call.scope,
                  subroutine_name,
                  subroutine_info.param_types[i],
                  i,
                  expr_type,
                  expr
                )
                .to_owned(),
              );
            }
          }
        }
      }

      if subroutine_info.param_types.len() != subroutine_call.params.len() {
        self.scopes.errors.push(
          error_fmt_file!(
            &file_info.name,
            &file_info.content,
            subroutine_call.pos,
            "(TypeChecker)",
            "Type mismatch in {} scope `{}`. Call `{}` \
              expected {} parameter(s) but the statement included {} \
              parameter(s).",
            subroutine_call.scope.subroutine_type,
            subroutine_call.scope,
            subroutine_name,
            subroutine_info.param_types.len(),
            subroutine_call.params.len()
          )
          .to_owned(),
        );
      }
    } else {
      self.scopes.errors.push(
        error_fmt_file!(
          &file_info.name,
          &file_info.content,
          subroutine_call.pos,
          "(TypeChecker)",
          "Could not find {} declaration `{}` in global scope.",
          subroutine_call.scope.subroutine_type,
          subroutine_name
        )
        .to_owned(),
      );
    }

    return Ok(());
  }
}
