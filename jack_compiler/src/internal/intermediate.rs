////////////////////////////////////////////////////////////////////////////////
// File: src/internal/intermediate.rs
// Description: Intermediate representation module
//
// Author: Leon Heidelbach <leon.heidelbach@hhu.de>
// Date: 15.05.2024
//
// License: GPLv3
////////////////////////////////////////////////////////////////////////////////

use super::{
  ast::*,
  check::GlobalScope,
  jack::{
    JackToken::{self, *},
    SymbolKind, VMSegment,
  },
  tokenize::Keyword,
};

use shared::{
  error_fmt_file, error_fmt_src, io::os::FileInfo,
  util::helpers::StrManipulation,
};

////////////////////////////////////////////////////////////////////////////////
// Helper structs
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub(crate) struct GeneratorInfo<'a> {
  pub(crate) file_info: &'a FileInfo,
  pub(crate) scopes: &'a GlobalScope,
}

impl<'a> GeneratorInfo<'a> {
  pub(crate) fn new(file_info: &'a FileInfo, scope: &'a GlobalScope) -> Self {
    return Self {
      file_info,
      scopes: scope,
    };
  }
}

////////////////////////////////////////////////////////////////////////////////
// Traits
////////////////////////////////////////////////////////////////////////////////

pub trait IntermediateRepresentation<'a> {
  type Data;
  type Output;
  fn intermediate(&self, data: &Self::Data) -> Self::Output;
}

////////////////////////////////////////////////////////////////////////////////
// Intermediate Representation
////////////////////////////////////////////////////////////////////////////////

impl<'a> IntermediateRepresentation<'a> for Program {
  type Data = GeneratorInfo<'a>;
  type Output = Vec<JackToken>;
  fn intermediate(&self, data: &Self::Data) -> Self::Output {
    return self
      .classes
      .iter()
      .flat_map(|c| return c.intermediate(data))
      .collect();
  }
}

impl<'a> IntermediateRepresentation<'a> for ClassDeclaration {
  type Data = GeneratorInfo<'a>;
  type Output = Vec<JackToken>;
  fn intermediate(&self, data: &Self::Data) -> Self::Output {
    let mut code = vec![comment(
      format!(" class declaration {}", self.identifier.name)
        .pad_sides_with("-", "-", 76)
        + " ;",
    )];

    code.extend(
      self
        .subroutines
        .iter()
        .flat_map(|f| return f.intermediate(data)),
    );

    return code;
  }
}

impl<'a> IntermediateRepresentation<'a> for SubroutineDeclaration {
  type Data = GeneratorInfo<'a>;
  type Output = Vec<JackToken>;
  fn intermediate(&self, data: &Self::Data) -> Self::Output {
    let current_scope = format!("{}.{}", self.class.name, self.identifier.name);
    let local_scope = &data.scopes.get_local_scope_map(
      &current_scope,
      data.file_info,
      self.pos,
    );
    let locals = local_scope.map.len() - self.params.len();
    let mut code: Vec<JackToken> = vec![
      comment(
        format!(
          " {} declaration: {} ",
          self.subroutine_type, self.identifier.name
        )
        .pad_sides_with("-", "-", 76)
          + " ;",
      ),
      function(current_scope.clone(), locals as u16),
    ];

    data.scopes.indexer.reset_subroutine_counters();

    match self.subroutine_type {
      Keyword::Constructor => {
        let field_count = data
          .scopes
          .class_vars
          .get(&self.class.name)
          .expect(error_fmt_src!(
            "Class `{}` not could not be found",
            self.class.name
          ))
          .get_count_of_kind(SymbolKind::Field);

        code.extend(vec![
          comment(
            format!(" allocate memory for {} fields ", field_count)
              .pad_sides_with("-", "-", 76)
              + " ;",
          ),
          push(VMSegment::Constant, field_count as u16),
          call(String::from("Memory.alloc"), 1),
          pop(VMSegment::Pointer, 0),
        ]);
      }
      Keyword::Method => {
        code.extend(vec![
          comment(
            "set this pointer to argument 0".pad_sides_with("-", "-", 76)
              + " ;",
          ),
          push(VMSegment::Argument, 0),
          pop(VMSegment::Pointer, 0),
        ]);
      }
      _ => (),
    }

    for statement in &self.body.statements {
      code.extend(statement.intermediate(data));
    }

    return code;
  }
}

impl<'a> IntermediateRepresentation<'a> for Statement {
  type Data = GeneratorInfo<'a>;
  type Output = Vec<JackToken>;
  fn intermediate(&self, data: &Self::Data) -> Self::Output {
    return match self {
      Statement::SubroutineCall(function_call) => {
        function_call.intermediate(data)
      }
      Statement::VariableAssignment(variable_assignment) => {
        variable_assignment.intermediate(data)
      }
      Statement::If(if_statement) => if_statement.intermediate(data),
      Statement::While(while_statement) => while_statement.intermediate(data),
      Statement::Return(return_statement) => {
        return_statement.intermediate(data)
      }
      _ => vec![],
    };
  }
}

impl<'a> IntermediateRepresentation<'a> for SubroutineCall {
  type Data = GeneratorInfo<'a>;
  type Output = Vec<JackToken>;
  fn intermediate(&self, data: &Self::Data) -> Self::Output {
    let mut code: Vec<JackToken> = Vec::new();
    let mut arg_count = self.params.len();

    if !self.has_class_specifier {
      code.push(push(VMSegment::Pointer, 0));
    }

    let subroutine_name = if let Some((idx, variable)) =
      data.scopes.indexer.get_var(
        &self.scope,
        &self.class_var_ref,
        self.has_this_class_specifier,
      ) {
      let segment = match variable.kind {
        SymbolKind::Static => VMSegment::Static,
        SymbolKind::Field => VMSegment::This,
        SymbolKind::Argument => VMSegment::Argument,
        SymbolKind::Local => VMSegment::Local,
      };

      code.push(push(segment, idx));

      format!("{}.{}", variable.r#type, self.identifier.name)
    } else {
      format!("{}.{}", self.class_var_ref.name, self.identifier.name)
    };

    let subroutine_info = &data
      .scopes
      .get_local_scope_map(&subroutine_name, data.file_info, self.pos)
      .subroutine_info;

    if !self.has_class_specifier {
      arg_count = subroutine_info.param_types.len();
    }

    if subroutine_info.r#type == Keyword::Method {
      arg_count += 1;
    }

    code.extend(
      self
        .params
        .iter()
        .flat_map(|param| return param.intermediate(data)),
    );
    code.push(call(subroutine_name, arg_count as u16));

    if self.call_type == Some(Keyword::Do) {
      code.push(pop(VMSegment::Temp, 0));
    }

    return code;
  }
}

impl<'a> IntermediateRepresentation<'a> for VariableAssignment {
  type Data = GeneratorInfo<'a>;
  type Output = Vec<JackToken>;
  fn intermediate(&self, data: &Self::Data) -> Self::Output {
    let mut code = vec![comment(
      format!(" variable assignment: {} ", self.identifier.name)
        .pad_sides_with("-", "-", 76)
        + " ;",
    )];

    let (idx, variable) = data
      .scopes
      .indexer
      .get_var(&self.scope, &self.identifier, self.is_class_var)
      .expect(error_fmt_file!(
        &data.file_info.name,
        &data.file_info.content,
        self.pos,
        "(CodeGenerator)",
        "Variable `{}` not found in scope `{}`",
        self.identifier.name,
        self.scope
      ));

    let segment = match variable.kind {
      SymbolKind::Static => VMSegment::Static,
      SymbolKind::Field => VMSegment::This,
      SymbolKind::Argument => VMSegment::Argument,
      SymbolKind::Local => VMSegment::Local,
    };

    if let Some(expr) = &self.identifier.array_offset {
      code.extend(expr.intermediate(data));
      code.extend(vec![push(segment, idx), add]);
      code.extend(self.value.intermediate(data));
      code.extend(vec![
        pop(VMSegment::Temp, 0),
        pop(VMSegment::Pointer, 1),
        push(VMSegment::Temp, 0),
        pop(VMSegment::That, 0),
      ]);
    } else {
      code.extend(self.value.intermediate(data));
      code.push(pop(segment, idx));
    }

    return code;
  }
}

impl<'a> IntermediateRepresentation<'a> for IfStatement {
  type Data = GeneratorInfo<'a>;
  type Output = Vec<JackToken>;
  fn intermediate(&self, data: &Self::Data) -> Self::Output {
    let label_one = data.scopes.indexer.new_label();
    let label_two = data.scopes.indexer.new_label();
    let mut code = vec![comment(
      " if statement begin ".pad_sides_with("-", "-", 76) + " ;",
    )];

    code.extend(self.condition.intermediate(data));
    code.extend(vec![
      not,
      if_goto(label_one.clone()),
      comment(" if condition ".pad_sides_with("-", "-", 76) + " ;"),
    ]);
    code.extend(
      self
        .if_body
        .statements
        .iter()
        .flat_map(|s| return s.intermediate(data)),
    );
    code.extend(vec![goto(label_two.clone()), label(label_one)]);

    if let Some(else_body) = &self.else_body {
      code.push(comment(
        " else statement begin ".pad_sides_with("-", "-", 76) + " ;",
      ));
      code.extend(
        else_body
          .statements
          .iter()
          .flat_map(|s| return s.intermediate(data)),
      );
    }

    code.extend(vec![
      label(label_two),
      comment(" if statement end ".pad_sides_with("-", "-", 76) + " ;"),
    ]);

    return code;
  }
}

impl<'a> IntermediateRepresentation<'a> for WhileStatement {
  type Data = GeneratorInfo<'a>;
  type Output = Vec<JackToken>;
  fn intermediate(&self, data: &Self::Data) -> Self::Output {
    let label_one = data.scopes.indexer.new_label();
    let label_two = data.scopes.indexer.new_label();
    let mut code = vec![
      comment(" while statement begin ".pad_sides_with("-", "-", 76) + " ;"),
      label(label_one.clone()),
    ];

    code.extend(self.condition.intermediate(data));
    code.extend(vec![
      not,
      if_goto(label_two.clone()),
      comment(" while condition ".pad_sides_with("-", "-", 76) + " ;"),
    ]);
    code.extend(
      self
        .body
        .statements
        .iter()
        .flat_map(|s| return s.intermediate(data)),
    );
    code.extend(vec![goto(label_one), label(label_two)]);

    return code;
  }
}

impl<'a> IntermediateRepresentation<'a> for ReturnStatement {
  type Data = GeneratorInfo<'a>;
  type Output = Vec<JackToken>;
  fn intermediate(&self, data: &Self::Data) -> Self::Output {
    let mut code = Vec::new();

    if let Some(expr) = &self.value {
      code.extend(expr.intermediate(data));
    } else {
      code.push(push(VMSegment::Constant, 0));
    }

    code.push(r#return);

    return code;
  }
}

impl<'a> IntermediateRepresentation<'a> for ExpressionInfo {
  type Data = GeneratorInfo<'a>;
  type Output = Vec<JackToken>;
  fn intermediate(&self, data: &Self::Data) -> Self::Output {
    let mut code = Vec::new();

    match &self.expr {
      Expression::Primary(primary) => code.extend(primary.intermediate(data)),
      Expression::Unary { op, ref rhs } => {
        let (op, _) = op;
        code.extend(rhs.intermediate(data));
        match op {
          Operator::Not => code.push(not),
          Operator::Sub => code.push(neg),
          _ => code.push(op.clone().into()),
        }
      }
      Expression::BinOp { lhs, op, rhs } => {
        let (op, _) = op;
        code.extend(lhs.intermediate(data));
        code.extend(rhs.intermediate(data));
        code.push(op.clone().into());
      }
    }

    return code;
  }
}

impl<'a> IntermediateRepresentation<'a> for Primary {
  type Data = GeneratorInfo<'a>;
  type Output = Vec<JackToken>;
  fn intermediate(&self, data: &Self::Data) -> Self::Output {
    return match self {
      Primary::Integer(integer) => integer.intermediate(data),
      Primary::Character(character) => character.intermediate(data),
      Primary::SizedString(string) => string.intermediate(data),
      Primary::Boolean(boolean) => boolean.intermediate(data),
      Primary::Null(_) => vec![push(VMSegment::Constant, 0)],
      Primary::This(_) => vec![push(VMSegment::Pointer, 0)],
      Primary::Identifier(identifier) => identifier.intermediate(data),
      Primary::SubroutineCall(function_call) => {
        function_call.intermediate(data)
      }
    };
  }
}

impl<'a> IntermediateRepresentation<'a> for Integer {
  type Data = GeneratorInfo<'a>;
  type Output = Vec<JackToken>;
  fn intermediate(&self, _data: &Self::Data) -> Self::Output {
    return vec![push(VMSegment::Constant, self.value)];
  }
}

impl<'a> IntermediateRepresentation<'a> for Character {
  type Data = GeneratorInfo<'a>;
  type Output = Vec<JackToken>;
  fn intermediate(&self, _data: &Self::Data) -> Self::Output {
    return vec![push(VMSegment::Constant, self.value)];
  }
}

impl<'a> IntermediateRepresentation<'a> for Boolean {
  type Data = GeneratorInfo<'a>;
  type Output = Vec<JackToken>;
  fn intermediate(&self, _data: &Self::Data) -> Self::Output {
    if self.value {
      return vec![push(VMSegment::Constant, 0), not];
    } else {
      return vec![push(VMSegment::Constant, 0)];
    }
  }
}

impl<'a> IntermediateRepresentation<'a> for SizedString {
  type Data = GeneratorInfo<'a>;
  type Output = Vec<JackToken>;
  fn intermediate(&self, _data: &Self::Data) -> Self::Output {
    let mut code = vec![
      push(VMSegment::Constant, self.value.len() as u16),
      call(String::from("String.new"), 1),
    ];

    self.value.bytes().for_each(|b| {
      code.extend(vec![
        push(VMSegment::Constant, b as u16),
        call(String::from("String.appendChar"), 2),
      ]);
    });

    return code;
  }
}

impl<'a> IntermediateRepresentation<'a> for Identifier {
  type Data = GeneratorInfo<'a>;
  type Output = Vec<JackToken>;
  fn intermediate(&self, data: &Self::Data) -> Self::Output {
    let mut code = Vec::new();

    let (idx, variable) = data
      .scopes
      .indexer
      .get_var(&self.scope, self, self.is_class_var)
      .expect(error_fmt_file!(
        &data.file_info.name,
        &data.file_info.content,
        self.pos,
        "(CodeGenerator)",
        "Missing index for variable `{}`",
        self.name
      ));

    let segment = match variable.kind {
      SymbolKind::Static => VMSegment::Static,
      SymbolKind::Field => VMSegment::This,
      SymbolKind::Argument => VMSegment::Argument,
      SymbolKind::Local => VMSegment::Local,
    };

    if let Some(expr) = &self.array_offset {
      code.extend(expr.intermediate(data));
      code.extend(vec![
        push(segment, idx),
        add,
        pop(VMSegment::Pointer, 1),
        push(VMSegment::That, 0),
      ]);
    } else {
      code.push(push(segment, idx));
    }

    return code;
  }
}
