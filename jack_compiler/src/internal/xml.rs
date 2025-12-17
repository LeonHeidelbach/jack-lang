////////////////////////////////////////////////////////////////////////////////
// File: src/internal/xml.rs
// Description: AST XML serialization
//
// Author: Leon Heidelbach <leon.heidelbach@hhu.de>
// Date: 15.05.2024
//
// License: GPLv3
////////////////////////////////////////////////////////////////////////////////

use super::{
  ast::*,
  jack::SymbolKind,
  tokenize::{Keyword, Symbol, Token},
};

use shared::error_panic;

////////////////////////////////////////////////////////////////////////////////
// Constants
////////////////////////////////////////////////////////////////////////////////

const XML_CLASS: &str = "class";
const XML_CLASS_VAR_DEC: &str = "classVarDec";
const XML_DO_STATEMENT: &str = "doStatement";
const XML_EXPRESSION: &str = "expression";
const XML_EXPRESSION_LIST: &str = "expressionList";
const XML_IDENTIFIER: &str = "identifier";
const XML_IF_STATEMENT: &str = "ifStatement";
const XML_INTEGER_CONSTANT: &str = "integerConstant";
const XML_CHARACTER_CONSTANT: &str = "characterConstant";
const XML_KEYWORD: &str = "keyword";
const XML_PARAMETER_LIST: &str = "parameterList";
const XML_RETURN_STATEMENT: &str = "returnStatement";
const XML_STATEMENTS: &str = "statements";
const XML_STRING_CONSTANT: &str = "stringConstant";
const XML_SUBROUTINE_BODY: &str = "subroutineBody";
const XML_SUBROUTINE_DEC: &str = "subroutineDec";
const XML_SYMBOL: &str = "symbol";
const XML_TERM: &str = "term";
const XML_TOKENS: &str = "tokens";
const XML_VAR_DEC: &str = "varDec";
const XML_WHILE_STATEMENT: &str = "whileStatement";

////////////////////////////////////////////////////////////////////////////////
// XML Tag Generation
////////////////////////////////////////////////////////////////////////////////

pub(crate) fn xml(
  tag: &str,
  content: &str,
  indent: usize,
  inline: bool,
) -> String {
  let spacing = if indent > 0 {
    " ".repeat(indent * 2)
  } else {
    String::new()
  };

  if inline {
    return format!("{}<{}> {} </{}>\n", spacing, tag, content, tag);
  }

  return format!("{}<{}>\n{}{}</{}>\n", spacing, tag, content, spacing, tag);
}

////////////////////////////////////////////////////////////////////////////////
// Traits
////////////////////////////////////////////////////////////////////////////////

pub(crate) trait XmlSerialize {
  type Output;
  fn to_xml(&self, indent: usize) -> Self::Output;
}

////////////////////////////////////////////////////////////////////////////////
// Helper Structs
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub(crate) struct OwnedXmlSerializableTokenStream {
  pub(crate) tokens: Vec<Token>,
}

impl From<Program> for OwnedXmlSerializableTokenStream {
  fn from(program: Program) -> Self {
    return Self {
      tokens: program.tokens,
    };
  }
}

#[derive(Debug, Clone)]
pub(crate) struct XmlSerializableTokenStream<'a> {
  pub(crate) tokens: &'a Vec<Token>,
}

impl<'a> From<&'a Program> for XmlSerializableTokenStream<'a> {
  fn from(program: &'a Program) -> Self {
    return Self {
      tokens: &program.tokens,
    };
  }
}

#[derive(Debug, Clone)]
pub(crate) struct XmlSerializableProgram {
  pub(crate) program: Program,
}

impl From<Program> for XmlSerializableProgram {
  fn from(program: Program) -> Self {
    return Self { program };
  }
}

////////////////////////////////////////////////////////////////////////////////
// Tokens, Symbols, Keywords & Types
////////////////////////////////////////////////////////////////////////////////

impl XmlSerialize for Token {
  type Output = String;
  fn to_xml(&self, indent: usize) -> Self::Output {
    return match self {
      Token::Identifier(ident, _) => xml(XML_IDENTIFIER, ident, indent, true),
      Token::IntegerConstant(int, _) => {
        xml(XML_INTEGER_CONSTANT, &int.to_string(), indent, true)
      }
      Token::CharacterConstant(char, _) => {
        xml(XML_CHARACTER_CONSTANT, &char.to_string(), indent, true)
      }
      Token::Keyword(kw, _) => kw.to_xml(indent),
      Token::StringConstant(str, _) => {
        xml(XML_STRING_CONSTANT, str, indent, true)
      }
      Token::Symbol(sym, _) => sym.to_xml(indent),
    };
  }
}

impl XmlSerialize for &[Token] {
  type Output = String;
  fn to_xml(&self, indent: usize) -> Self::Output {
    return xml(
      XML_TOKENS,
      &self
        .iter()
        .fold(String::new(), |acc, x| return acc + &x.to_xml(indent)),
      indent,
      false,
    );
  }
}

impl XmlSerialize for Symbol {
  type Output = String;
  fn to_xml(&self, indent: usize) -> Self::Output {
    return xml(XML_SYMBOL, &self.to_string(), indent, true);
  }
}

impl XmlSerialize for Keyword {
  type Output = String;
  fn to_xml(&self, indent: usize) -> Self::Output {
    return xml(XML_KEYWORD, &self.to_string(), indent, true);
  }
}

impl XmlSerialize for Type {
  type Output = String;
  fn to_xml(&self, indent: usize) -> Self::Output {
    return match self {
      Type::Integer => Keyword::Int.to_xml(indent),
      Type::Character => Keyword::Char.to_xml(indent),
      Type::Boolean => Keyword::Boolean.to_xml(indent),
      Type::Void => Keyword::Void.to_xml(indent),
      Type::AnyComplex => Keyword::Any.to_xml(indent),
      Type::Complex(c) => xml(XML_IDENTIFIER, c, indent, true),
    };
  }
}

////////////////////////////////////////////////////////////////////////////////
// Program
////////////////////////////////////////////////////////////////////////////////

impl XmlSerialize for Program {
  type Output = String;
  fn to_xml(&self, indent: usize) -> String {
    return self.classes.iter().fold(String::new(), |acc, class| {
      return acc + &class.to_xml(indent);
    });
  }
}

////////////////////////////////////////////////////////////////////////////////
// Declarations & Assignments
////////////////////////////////////////////////////////////////////////////////

impl XmlSerialize for ClassDeclaration {
  type Output = String;
  fn to_xml(&self, indent: usize) -> String {
    let class_vars = self.class_vars.iter().fold(String::new(), |acc, var| {
      return acc + &var.to_xml(indent + 1);
    });

    let subroutines =
      self
        .subroutines
        .iter()
        .fold(String::new(), |acc, subroutine| {
          return acc + &subroutine.to_xml(indent + 1);
        });

    let class_body = format!(
      "{}{}{}{}{}{}",
      Keyword::Class.to_xml(indent + 1),
      self.identifier.to_xml(indent + 1),
      Symbol::LeftCurlyBrace.to_xml(indent + 1),
      class_vars,
      subroutines,
      Symbol::RightCurlyBrace.to_xml(indent + 1)
    );

    return xml(XML_CLASS, &class_body, indent, false);
  }
}

impl XmlSerialize for ClassVariableDeclaration {
  type Output = String;
  fn to_xml(&self, indent: usize) -> String {
    let identifiers_len = self.identifiers.len();
    let comma = Symbol::Comma.to_xml(indent + 1);

    let identifiers = self.identifiers.iter().enumerate().fold(
      String::new(),
      |mut acc, (i, identifier)| {
        acc += &identifier.to_xml(indent + 1);

        if i < identifiers_len - 1 {
          acc += &comma;
        }

        return acc;
      },
    );

    let class_var_dec = format!(
      "{}{}{}{}",
      self.var_type.to_xml(indent + 1),
      self.type_dec.to_xml(indent + 1),
      identifiers,
      Symbol::Semicolon.to_xml(indent + 1)
    );

    return xml(XML_CLASS_VAR_DEC, &class_var_dec, indent, false);
  }
}

impl XmlSerialize for SubroutineDeclaration {
  type Output = String;
  fn to_xml(&self, indent: usize) -> String {
    let param_len = self.params.len();
    let comma = Symbol::Comma.to_xml(indent + 2);

    let (var_decs, body): (Vec<_>, Vec<_>) = self
      .body
      .statements
      .iter()
      .partition(|x| matches!(x, Statement::VariableDeclaration(_)));

    let parameter_list = self.params.iter().enumerate().fold(
      String::new(),
      |mut acc, (i, param)| {
        acc += &param.to_xml(indent + 1);

        if i < param_len - 1 {
          acc += &comma;
        }

        return acc;
      },
    );

    let var_declarations =
      var_decs.iter().fold(String::new(), |acc, statement| {
        return acc + &statement.to_xml(indent + 2);
      });

    let body_statements = body.iter().fold(String::new(), |acc, statement| {
      return acc + &statement.to_xml(indent + 3);
    });

    let subroutine_body = format!(
      "{}{}{}{}",
      Symbol::LeftCurlyBrace.to_xml(indent + 2),
      var_declarations,
      xml(XML_STATEMENTS, &body_statements, indent + 2, false),
      Symbol::RightCurlyBrace.to_xml(indent + 2)
    );

    return xml(
      XML_SUBROUTINE_DEC,
      &format!(
        "{}{}{}{}{}{}{}",
        self.subroutine_type.to_xml(indent + 1),
        self.return_type.to_xml(indent + 1),
        self.identifier.to_xml(indent + 1),
        Symbol::LeftParen.to_xml(indent + 1),
        xml(XML_PARAMETER_LIST, &parameter_list, indent + 1, false),
        Symbol::RightParen.to_xml(indent + 1),
        xml(XML_SUBROUTINE_BODY, &subroutine_body, indent + 1, false)
      ),
      indent,
      false,
    );
  }
}

impl XmlSerialize for SubroutineDeclarationParam {
  type Output = String;
  fn to_xml(&self, indent: usize) -> String {
    return format!(
      "{}{}",
      &self.type_dec.to_xml(indent + 1),
      &self.identifier.to_xml(indent + 1),
    );
  }
}

impl XmlSerialize for TypeDeclaration {
  type Output = String;
  fn to_xml(&self, indent: usize) -> String {
    return self.raw_type.to_xml(indent);
  }
}

impl XmlSerialize for VariableDeclaration {
  type Output = String;
  fn to_xml(&self, indent: usize) -> String {
    let identifiers_len = self.identifiers.len();
    let comma = Symbol::Comma.to_xml(indent + 1);

    let identifiers = self.identifiers.iter().enumerate().fold(
      String::new(),
      |mut acc, (i, identifier)| {
        acc += &identifier.to_xml(indent + 1);
        if i < identifiers_len - 1 {
          acc += &comma;
        }
        return acc;
      },
    );

    let var_dec = format!(
      "{}{}{}{}",
      Keyword::Var.to_xml(indent + 1),
      self.type_dec.to_xml(indent + 1),
      identifiers,
      Symbol::Semicolon.to_xml(indent + 1)
    );

    return xml(XML_VAR_DEC, &var_dec, indent, false);
  }
}

impl XmlSerialize for VariableAssignment {
  type Output = String;
  fn to_xml(&self, indent: usize) -> String {
    return xml(
      "letStatement",
      &format!(
        "{}{}{}{}{}",
        Keyword::Let.to_xml(indent + 1),
        self.identifier.to_xml(indent + 1),
        Symbol::Eq.to_xml(indent + 1),
        xml(
          XML_EXPRESSION,
          &self.value.to_xml(indent + 2),
          indent + 1,
          false
        ),
        Symbol::Semicolon.to_xml(indent + 1),
      ),
      indent,
      false,
    );
  }
}

////////////////////////////////////////////////////////////////////////////////
// Statements
////////////////////////////////////////////////////////////////////////////////

impl XmlSerialize for Statement {
  type Output = String;
  fn to_xml(&self, indent: usize) -> String {
    return match self {
      Statement::Return(ret) => ret.to_xml(indent),
      Statement::VariableDeclaration(var_decl) => var_decl.to_xml(indent),
      Statement::VariableAssignment(var_assign) => var_assign.to_xml(indent),
      Statement::If(r#if) => r#if.to_xml(indent),
      Statement::While(r#while) => r#while.to_xml(indent),
      Statement::SubroutineCall(fun_call) => fun_call.to_xml(indent),
    };
  }
}

impl XmlSerialize for IfStatement {
  type Output = String;
  fn to_xml(&self, indent: usize) -> String {
    let mut if_statement = String::new();

    if_statement += &format!(
      "{}{}{}{}{}",
      Keyword::If.to_xml(indent + 1),
      Symbol::LeftParen.to_xml(indent + 1),
      xml(
        XML_EXPRESSION,
        &self.condition.to_xml(indent + 2),
        indent + 1,
        false
      ),
      Symbol::RightParen.to_xml(indent + 1),
      Symbol::LeftCurlyBrace.to_xml(indent + 1)
    );

    let if_body =
      self
        .if_body
        .statements
        .iter()
        .fold(String::new(), |acc, statement| {
          return acc + &statement.to_xml(indent + 2);
        });

    if_statement += &xml(XML_STATEMENTS, &if_body, indent + 1, false);
    if_statement += &Symbol::RightCurlyBrace.to_xml(indent + 1);

    if let Some(statements) = &self.else_body {
      if_statement += &format!(
        "{}{}",
        Keyword::Else.to_xml(indent + 1),
        Symbol::LeftCurlyBrace.to_xml(indent + 1)
      );

      let else_body =
        statements
          .statements
          .iter()
          .fold(String::new(), |acc, statement| {
            return acc + &statement.to_xml(indent + 2);
          });

      if_statement += &xml(XML_STATEMENTS, &else_body, indent + 1, false);
      if_statement += &Symbol::RightCurlyBrace.to_xml(indent + 1);
    }

    return xml(XML_IF_STATEMENT, &if_statement, indent, false);
  }
}

impl XmlSerialize for WhileStatement {
  type Output = String;
  fn to_xml(&self, indent: usize) -> String {
    let while_body =
      self
        .body
        .statements
        .iter()
        .fold(String::new(), |mut acc, statement| {
          acc.push_str(&statement.to_xml(indent + 2));
          return acc;
        });

    let while_statement = format!(
      "{}{}{}{}{}{}{}",
      Keyword::While.to_xml(indent + 1),
      Symbol::LeftParen.to_xml(indent + 1),
      xml(
        XML_EXPRESSION,
        &self.condition.to_xml(indent + 2),
        indent + 1,
        false
      ),
      Symbol::RightParen.to_xml(indent + 1),
      Symbol::LeftCurlyBrace.to_xml(indent + 1),
      xml(XML_STATEMENTS, &while_body, indent + 1, false),
      Symbol::RightCurlyBrace.to_xml(indent + 1)
    );

    return xml(XML_WHILE_STATEMENT, &while_statement, indent, false);
  }
}

impl XmlSerialize for ReturnStatement {
  type Output = String;
  fn to_xml(&self, indent: usize) -> String {
    let mut ret_statement = Keyword::Return.to_xml(indent + 1);

    if let Some(expr) = &self.value {
      ret_statement +=
        &xml(XML_EXPRESSION, &expr.to_xml(indent + 2), indent + 1, false);
    }

    ret_statement += &Symbol::Semicolon.to_xml(indent + 1);

    return xml(XML_RETURN_STATEMENT, &ret_statement, indent, false);
  }
}

////////////////////////////////////////////////////////////////////////////////
// Subroutine Calls
////////////////////////////////////////////////////////////////////////////////

impl XmlSerialize for SubroutineCall {
  type Output = String;
  fn to_xml(&self, indent: usize) -> String {
    let is_do_call = Some(Keyword::Do) == self.call_type;
    let adjusted_indent = if is_do_call { indent + 1 } else { indent };
    let param_len = self.params.len();
    let comma = Symbol::Comma.to_xml(adjusted_indent + 1);

    let mut sub_call = String::new();

    if self.has_class_specifier {
      sub_call += &self.class_var_ref.to_xml(adjusted_indent);
      sub_call += &Symbol::Dot.to_xml(adjusted_indent);
    }

    sub_call += &self.identifier.to_xml(adjusted_indent);
    sub_call += &Symbol::LeftParen.to_xml(adjusted_indent);

    let mut expr_list = String::new();

    for (i, arg) in self.params.iter().enumerate() {
      expr_list += &xml(
        XML_EXPRESSION,
        &arg.to_xml(adjusted_indent + 2),
        adjusted_indent + 1,
        false,
      );

      if i < param_len - 1 {
        expr_list += &comma;
      }
    }

    sub_call += &xml(XML_EXPRESSION_LIST, &expr_list, adjusted_indent, false);
    sub_call += &Symbol::RightParen.to_xml(adjusted_indent);

    if is_do_call {
      let mut do_statement = String::new();

      do_statement += &Keyword::Do.to_xml(adjusted_indent);
      do_statement += &sub_call;
      do_statement += &Symbol::Semicolon.to_xml(adjusted_indent);

      return xml(XML_DO_STATEMENT, &do_statement, indent, false);
    }

    return sub_call;
  }
}

////////////////////////////////////////////////////////////////////////////////
// Expressions
////////////////////////////////////////////////////////////////////////////////

impl XmlSerialize for ExpressionInfo {
  type Output = String;
  fn to_xml(&self, indent: usize) -> String {
    match &self.expr {
      Expression::Primary(_) => {
        return xml(
          XML_TERM,
          &if self.bracketed {
            format!(
              "{}{}{}",
              Symbol::LeftParen.to_xml(indent + 1),
              self.expr.to_xml(indent),
              Symbol::RightParen.to_xml(indent + 1),
            )
          } else {
            self.expr.to_xml(indent)
          },
          indent,
          false,
        );
      }
      Expression::Unary { .. } => {
        let mut unary_expr = String::new();

        if self.bracketed {
          unary_expr += &Symbol::LeftParen.to_xml(indent + 1);
          unary_expr += &xml(
            XML_EXPRESSION,
            &xml(XML_TERM, &self.expr.to_xml(indent + 3), indent + 2, false),
            indent + 1,
            false,
          );
          unary_expr += &Symbol::RightParen.to_xml(indent + 1);
        } else {
          unary_expr += &self.expr.to_xml(indent + 1);
        }

        return xml(XML_TERM, &unary_expr, indent, false);
      }
      Expression::BinOp { .. } => {
        let indent = if self.bracketed { indent + 1 } else { indent };
        let mut binop_expr = String::new();

        if self.bracketed {
          binop_expr += &Symbol::LeftParen.to_xml(indent);
          binop_expr +=
            &xml(XML_EXPRESSION, &self.expr.to_xml(indent + 1), indent, false);
          binop_expr += &Symbol::RightParen.to_xml(indent);

          return xml(XML_TERM, &binop_expr, indent - 1, false);
        } else {
          binop_expr += &self.expr.to_xml(indent);

          return binop_expr;
        }
      }
    }
  }
}

impl XmlSerialize for Expression {
  type Output = String;
  fn to_xml(&self, indent: usize) -> String {
    return match self {
      Expression::Primary(primary) => primary.to_xml(indent + 1),
      Expression::Unary { op, rhs } => {
        let (op, _) = op;

        format!("{}{}", op.to_xml(indent), rhs.to_xml(indent))
      }
      Expression::BinOp { lhs, op, rhs } => {
        let (op, _) = op;

        format!(
          "{}{}{}",
          lhs.to_xml(indent),
          op.to_xml(indent),
          rhs.to_xml(indent)
        )
      }
    };
  }
}

////////////////////////////////////////////////////////////////////////////////
// Primary Expression Elements
////////////////////////////////////////////////////////////////////////////////

impl XmlSerialize for Primary {
  type Output = String;
  fn to_xml(&self, indent: usize) -> String {
    return match self {
      Primary::Boolean(bool) => bool.to_xml(indent),
      Primary::SubroutineCall(fun_call) => fun_call.to_xml(indent),
      Primary::Identifier(identifier) => identifier.to_xml(indent),
      Primary::Integer(int) => int.to_xml(indent),
      Primary::Character(char) => char.to_xml(indent),
      Primary::Null(null) => null.to_xml(indent),
      Primary::SizedString(string) => string.to_xml(indent),
      Primary::This(this) => this.to_xml(indent),
    };
  }
}

////////////////////////////////////////////////////////////////////////////////
// Operators, Constants & Identifiers
////////////////////////////////////////////////////////////////////////////////

impl XmlSerialize for Operator {
  type Output = String;
  fn to_xml(&self, indent: usize) -> String {
    let xml_rep = match self {
      Operator::Mul => "*",
      Operator::Div => "/",
      Operator::Add => "+",
      Operator::Sub => "-",
      Operator::Not => "~",
      Operator::Eq => "=",
      Operator::Lt => "&lt;",
      Operator::Gt => "&gt;",
      Operator::And => "&amp;",
      Operator::Or => "|",
    };

    return xml(XML_SYMBOL, xml_rep, indent, true);
  }
}

impl XmlSerialize for Integer {
  type Output = String;
  fn to_xml(&self, indent: usize) -> String {
    return xml(XML_INTEGER_CONSTANT, &self.value.to_string(), indent, true);
  }
}

impl XmlSerialize for Character {
  type Output = String;
  fn to_xml(&self, indent: usize) -> String {
    return xml(
      XML_CHARACTER_CONSTANT,
      &self.value.to_string(),
      indent,
      true,
    );
  }
}

impl XmlSerialize for SizedString {
  type Output = String;
  fn to_xml(&self, indent: usize) -> String {
    return xml(XML_STRING_CONSTANT, &self.value, indent, true);
  }
}

impl XmlSerialize for Boolean {
  type Output = String;
  fn to_xml(&self, indent: usize) -> String {
    return xml(XML_KEYWORD, &self.value.to_string(), indent, true);
  }
}

impl XmlSerialize for Null {
  type Output = String;
  fn to_xml(&self, indent: usize) -> String {
    return Keyword::Null.to_xml(indent);
  }
}

impl XmlSerialize for This {
  type Output = String;
  fn to_xml(&self, indent: usize) -> String {
    return Keyword::This.to_xml(indent);
  }
}

impl XmlSerialize for Identifier {
  type Output = String;
  fn to_xml(&self, indent: usize) -> String {
    if let Some(index) = &self.array_offset {
      return format!(
        "{}{}{}{}",
        xml(XML_IDENTIFIER, &self.name, indent, true),
        Symbol::LeftSquareBracket.to_xml(indent),
        xml(XML_EXPRESSION, &index.to_xml(indent + 1), indent, false),
        Symbol::RightSquareBracket.to_xml(indent),
      );
    }

    return xml(XML_IDENTIFIER, &self.name, indent, true);
  }
}

impl XmlSerialize for SymbolKind {
  type Output = String;
  fn to_xml(&self, indent: usize) -> String {
    return match self {
      SymbolKind::Field => Keyword::Field.to_xml(indent),
      SymbolKind::Local => Keyword::Var.to_xml(indent),
      SymbolKind::Static => Keyword::Static.to_xml(indent),
      _ => error_panic!("Invalid symbol kind"),
    };
  }
}
