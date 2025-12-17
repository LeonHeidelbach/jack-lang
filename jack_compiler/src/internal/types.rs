////////////////////////////////////////////////////////////////////////////////
// File: src/internal/types.rs
// Description: Type extraction
//
// Author: Leon Heidelbach <leon.heidelbach@hhu.de>
// Date: 15.05.2024
//
// License: GPLv3
////////////////////////////////////////////////////////////////////////////////

use super::{
  ast::{Expression, ExpressionInfo, Operator, Primary, Type},
  check::GlobalScope,
  tokenize::{KEYWORD_NULL, KEYWORD_THIS},
};

use anyhow::Result;
use shared::{error_fmt, error_fmt_file, io::os::FileInfo};

////////////////////////////////////////////////////////////////////////////////
// Constants
////////////////////////////////////////////////////////////////////////////////

// Built-in complex types
pub(crate) const BUILTIN_TYPE_STRING: &str = "String";
pub(crate) const BUILTIN_TYPE_ARRAY: &str = "Array";
pub(crate) const BUILTIN_TYPE_THIS: &str = KEYWORD_THIS;
pub(crate) const BUILTIN_TYPE_NULL: &str = KEYWORD_NULL;

////////////////////////////////////////////////////////////////////////////////
// Traits
////////////////////////////////////////////////////////////////////////////////

pub(crate) trait ExtractTypes {
  fn types(
    &self,
    global_scope: &GlobalScope,
    file_info: &FileInfo,
  ) -> Result<Vec<(String, (Type, usize))>>;
}

////////////////////////////////////////////////////////////////////////////////
// Implementations
////////////////////////////////////////////////////////////////////////////////

impl ExtractTypes for ExpressionInfo {
  fn types(
    &self,
    global_scope: &GlobalScope,
    file_info: &FileInfo,
  ) -> Result<Vec<(String, (Type, usize))>> {
    let r#types = self.expr.types(global_scope, file_info);

    if let Ok(r#types) = &r#types {
      let (_, (first_type, _)) = &r#types.first().expect(error_fmt!(
        "No types found for expression `{:?}`.",
        self.expr
      ));
      self.expr_type.replace(first_type.clone());
    }

    return r#types;
  }
}

impl ExtractTypes for Expression {
  fn types(
    &self,
    global_scope: &GlobalScope,
    file_info: &FileInfo,
  ) -> Result<Vec<(String, (Type, usize))>> {
    return match self {
      //////////////////////////////////////////////////////////////////////////
      // Primary Expressions
      //////////////////////////////////////////////////////////////////////////
      Expression::Primary(ref primary) => match primary {
        Primary::Integer(ref val) => {
          return Ok(vec![(
            format!("Integer literal (value = `{}`)", val.value),
            (Type::Integer, val.pos),
          )]);
        }
        Primary::Character(ref val) => {
          return Ok(vec![(
            format!("Character literal (value = `{}`)", val.character),
            (Type::Character, val.pos),
          )]);
        }
        Primary::SizedString(ref val) => {
          return Ok(vec![(
            format!("String literal (value = `{}`)", val.value),
            (Type::Complex(String::from(BUILTIN_TYPE_STRING)), val.pos),
          )]);
        }
        Primary::Boolean(ref val) => {
          return Ok(vec![(
            format!("Boolean literal (value = `{}`)", val.value),
            (Type::Boolean, val.pos),
          )])
        }
        Primary::This(ref val) => {
          let class_name = val.scope.class_name.clone();

          return Ok(vec![(
            format!(
              "Object self reference `{}` (value = `{}`)",
              BUILTIN_TYPE_THIS, class_name
            ),
            (Type::Complex(class_name), val.pos),
          )]);
        }
        Primary::Null(ref val) => {
          return Ok(vec![(
            format!("Object reference `{}`", BUILTIN_TYPE_NULL),
            (Type::Complex(String::from(BUILTIN_TYPE_NULL)), val.pos),
          )])
        }
        Primary::Identifier(ref ident) => {
          let result = global_scope.get_var_info(
            &ident.scope,
            ident,
            ident.is_class_var,
            file_info,
          );
          match result {
            Ok(variable) => {
              return Ok(vec![(
                format!("identifier `{}`", ident.name),
                (variable.r#type.clone(), ident.pos),
              )]);
            }
            Err(err) => {
              return Err(err);
            }
          }
        }
        Primary::SubroutineCall(ref sub) => {
          let subroutine_name = if let Ok(variable) = global_scope.get_var_info(
            &sub.scope,
            &sub.class_var_ref,
            sub.has_this_class_specifier,
            file_info,
          ) {
            format!("{}.{}", variable.r#type, sub.identifier.name)
          } else {
            format!("{}.{}", sub.class_var_ref.name, sub.identifier.name)
          };
          let is_intrinsic =
            global_scope.intrinsics.contains_key(&subroutine_name);
          let mut search_map = &global_scope.map;

          if is_intrinsic {
            search_map = global_scope.intrinsics;
          }

          if let Some(function) = search_map.get(&subroutine_name) {
            let subroutine_info = &function.subroutine_info;

            for (i, param) in sub.params.iter().enumerate() {
              let expr_types = param.types(global_scope, file_info)?;

              for (expr, (expr_type, expr_pos)) in expr_types {
                if let Some(param_type) = subroutine_info.param_types.get(i) {
                  if expr_type != *param_type
                    && !expr_type
                      .is_coerceable(param_type)
                      .safe_unwrap(file_info, expr_pos)
                  {
                    return Err(anyhow::anyhow!(error_fmt_file!(
                      &file_info.name,
                      &file_info.content,
                      expr_pos,
                      "(TypeChecker)",
                      "Type mismatch in {} scope `{}`. Call \
                        `{}` expected parameter of type `{}` at index {} \
                        but got `{}`. Found: {}.",
                      sub.scope.subroutine_type,
                      sub.scope,
                      subroutine_name,
                      subroutine_info.param_types[i],
                      i,
                      expr_type,
                      expr
                    )
                    .to_owned()));
                  }
                }
              }
            }

            if subroutine_info.param_types.len() != sub.params.len() {
              return Err(anyhow::anyhow!(error_fmt_file!(
                &file_info.name,
                &file_info.content,
                sub.pos,
                "(TypeChecker)",
                "Type mismatch in {} scope `{}`. Call `{}` \
                  expected {} parameter(s) but the statement included {} \
                  parameter(s).",
                sub.scope.subroutine_type,
                sub.scope,
                subroutine_name,
                subroutine_info.param_types.len(),
                sub.params.len()
              )
              .to_owned()));
            }

            return Ok(vec![(
              if is_intrinsic {
                format!(
                  "intrinsic `{}` with return type `{}`",
                  subroutine_name, subroutine_info.return_type
                )
              } else {
                format!(
                  "function `{}` with return type `{}`",
                  subroutine_name, subroutine_info.return_type
                )
              },
              (subroutine_info.return_type.to_owned(), sub.pos),
            )]);
          }

          return Err(anyhow::anyhow!(error_fmt_file!(
            &file_info.name,
            &file_info.content,
            sub.pos,
            "(TypeChecker)",
            "No declaration found for {} `{}` in scope of {} `{}`.",
            sub.scope.subroutine_type,
            subroutine_name,
            sub.scope.subroutine_type,
            sub.scope
          )
          .to_owned()));
        }
      },
      //////////////////////////////////////////////////////////////////////////
      // Unary Expressions
      //////////////////////////////////////////////////////////////////////////
      Expression::Unary { op, rhs } => {
        let types = rhs.types(global_scope, file_info)?;
        let (op, _) = op;

        if *op != Operator::Not {
          return Ok(types);
        }

        if let Some((desc, (raw_type, pos))) =
          types.iter().find(|(_, (tp, pos))| {
            return *tp != Type::Boolean
              && !tp
                .is_coerceable(&Type::Boolean)
                .safe_unwrap(file_info, *pos);
          })
        {
          return Err(anyhow::anyhow!(error_fmt_file!(
            &file_info.name,
            &file_info.content,
            *pos,
            "(TypeChecker)",
            "Unary operator `{}` on {} of type `{}` is not supported.",
            op.to_string(),
            desc,
            raw_type
          )
          .to_owned()));
        }

        Ok(types)
      }
      //////////////////////////////////////////////////////////////////////////
      // Binary Expressions
      //////////////////////////////////////////////////////////////////////////
      Expression::BinOp { lhs, op, rhs } => {
        let mut types: Vec<(String, (Type, usize))> = Vec::new();
        let (op, op_pos) = op;

        types.append(&mut lhs.types(global_scope, file_info)?);
        types.append(&mut rhs.types(global_scope, file_info)?);

        let (first_type_string, (first_type, _)) = &types[0];
        let (second_type_string, (second_type, second_type_pos)) = &types[1];

        if let Some((desc, (raw_type, _))) =
          types.iter().find(|(_, (tp, pos))| {
            return *tp != *first_type
              && !tp.is_coerceable(first_type).safe_unwrap(file_info, *pos);
          })
        {
          if op.is_comparison_op() {
            return Err(anyhow::anyhow!(error_fmt_file!(
              &file_info.name,
              &file_info.content,
              *op_pos,
              "(TypeChecker)",
              "Logical expression with different types `{}` and `{}` of \
                type `{}`.",
              desc,
              first_type_string,
              first_type
            )
            .to_owned()));
          }

          return Err(anyhow::anyhow!(error_fmt_file!(
            &file_info.name,
            &file_info.content,
            *op_pos,
            "(TypeChecker)",
            "Operator `{}` on {} of type `{}` and {} of type `{}` is not \
              supported.",
            op.to_string(),
            desc,
            raw_type,
            first_type_string,
            first_type
          )
          .to_owned()));
        }

        let string_type = Type::Complex(String::from(BUILTIN_TYPE_STRING));

        if (((*first_type == string_type || *second_type == string_type)
          && !op.is_valid_string_op())
          || ((*first_type == Type::Boolean || *second_type == Type::Boolean)
            && !op.is_valid_boolean_op()))
          || !first_type
            .is_coerceable(second_type)
            .safe_unwrap(file_info, *second_type_pos)
        {
          return Err(anyhow::anyhow!(error_fmt_file!(
            &file_info.name,
            &file_info.content,
            *op_pos,
            "(TypeChecker)",
            "Operator `{}` on {} of type `{}` and {} of type `{}` is not \
              supported.",
            op.to_string(),
            first_type_string,
            first_type,
            second_type_string,
            second_type
          )
          .to_owned()));
        }

        if op.is_comparison_op() {
          return Ok(vec![(
            format!("logical expression with operator `{}`", op),
            (Type::Boolean, *op_pos),
          )]);
        }

        Ok(types)
      }
    };
  }
}
