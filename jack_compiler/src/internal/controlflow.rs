////////////////////////////////////////////////////////////////////////////////
// File: src/internal/controlflow.rs
// Description: Intermediate representation module
//
// Author: Leon Heidelbach <leon.heidelbach@hhu.de>
// Date: 12.06.2024
//
// License: GPLv3
////////////////////////////////////////////////////////////////////////////////

use std::{
  cell::Cell,
  collections::{HashMap, HashSet},
};

use super::{
  ast::{
    Block, ClassDeclaration, ClassVariableDeclaration, Expression,
    ExpressionInfo, Primary, Program, Statement, SubroutineDeclaration,
    SubroutineDeclarationParam,
  },
  jack::SymbolKind,
  tokenize::Keyword,
};

use shared::{error_fmt, error_fmt_src, info_print};

////////////////////////////////////////////////////////////////////////////////
// Liveness Analysis
////////////////////////////////////////////////////////////////////////////////

struct Liveness<'a> {
  def: Vec<Option<HashSet<&'a str>>>,
  r#use: Vec<Option<HashSet<&'a str>>>,
  r#in: Vec<HashSet<&'a str>>,
  out: Vec<HashSet<&'a str>>,
}

impl Default for Liveness<'_> {
  fn default() -> Self {
    return Self {
      def: vec![],
      r#use: vec![],
      r#in: vec![],
      out: vec![],
    };
  }
}

impl<'a> Liveness<'a> {
  fn get_min_register_count(
    &mut self,
    cf_graph: &'a HashMap<String, ControlFlowGraph<'a>>,
  ) -> HashMap<&'a str, usize> {
    return cf_graph
      .iter()
      .fold(HashMap::new(), |mut acc, (name, graph)| {
        self._extract_defs_and_uses(graph);
        self._calculate_out_in(graph);

        acc.insert(name, self._graph_coloring());

        self.def.clear();
        self.r#use.clear();
        self.r#in.clear();

        return acc;
      });
  }

  fn _graph_coloring(&self) -> usize {
    let mut connections: HashMap<&str, HashSet<&str>> = HashMap::new();

    // 1. Build the interference graph
    for out_set in &self.out {
      for ident in out_set {
        let entry = connections.entry(ident).or_default();

        for out_ident in out_set {
          if ident != out_ident {
            entry.insert(out_ident);
          }
        }
      }
    }

    // 2. Sort nodes by degree in descending order
    let mut sorted: Vec<(&&str, usize)> = connections
      .iter()
      .map(|(k, v)| return (k, v.len()))
      .collect();
    sorted.sort_by(|a, b| return b.1.cmp(&a.1));

    // 3. Graph coloring
    let mut colors: HashMap<&str, usize> = HashMap::new();

    for (ident, _) in sorted {
      let mut used_colors = HashSet::new();

      // Find all colors used by adjacent nodes
      if let Some(neighbors) = connections.get(*ident) {
        for neighbor in neighbors {
          if let Some(color) = colors.get(neighbor) {
            used_colors.insert(*color);
          }
        }
      }

      // Assign the lowest possible color
      let mut color = 0;
      while used_colors.contains(&color) {
        color += 1;
      }

      colors.insert(*ident, color);
    }

    // 4. Return the number of colors used
    return colors.values().max().map_or(0, |v| return v + 1);
  }

  fn _calculate_out_in(&mut self, cf_graph: &ControlFlowGraph) {
    let def_len = self.def.len();

    self.r#in = vec![HashSet::new(); def_len];
    self.out = vec![HashSet::new(); def_len];

    loop {
      let old_out = self.out.clone();
      let old_in = self.r#in.clone();

      for i in (0..def_len).rev() {
        let out = self._get_out_for_index(i, cf_graph);
        self.out.get_mut(i).unwrap().extend(out);

        let r#in = self._get_in_for_index(i);
        self
          .r#in
          .get_mut(i)
          .expect(error_fmt!(
            "(LivenessError) Index in `in` is out of bounds!"
          ))
          .extend(r#in);
      }

      if old_in == self.r#in && old_out == self.out {
        break;
      }
    }
  }

  fn _get_out_for_index(
    &self,
    index: usize,
    cf_graph: &ControlFlowGraph,
  ) -> HashSet<&'a str> {
    let mut out = HashSet::new();

    if let Some(successors) =
      cf_graph.nodes.get(index).map(|n| return &n.successors)
    {
      for successor in successors {
        if let Some(in_set) = self.r#in.get(*successor) {
          out.extend(in_set.clone());
        }
      }
    }

    return out;
  }

  fn _get_in_for_index(&self, index: usize) -> HashSet<&'a str> {
    let inner: HashSet<&str> = self
      .out
      .get(index)
      .expect(error_fmt!(
        "(LivenessError) Index in `out` is out of bounds!"
      ))
      .difference(
        self
          .def
          .get(index)
          .expect(error_fmt!(
            "(LivenessError) Index in `def` is out of bounds!"
          ))
          .as_ref()
          .unwrap_or(&HashSet::new()),
      )
      .cloned()
      .collect();

    return self
      .r#use
      .get(index)
      .expect(error_fmt!(
        "(LivenessError) Index in `use` is out of bounds!"
      ))
      .as_ref()
      .unwrap_or(&HashSet::new())
      .union(&inner)
      .cloned()
      .collect();
  }

  fn _extract_defs_and_uses(&mut self, cf_graph: &'a ControlFlowGraph) {
    for node in &cf_graph.nodes {
      match node.content {
        ControlFlowNodeContent::Statement(Statement::Return(
          return_statement,
        )) => {
          self.def.push(None);

          if let Some(expression) = &return_statement.value {
            self.r#use.push(Self::_extract_uses_from_expr(expression));
          } else {
            self.r#use.push(None);
          }
        }
        ControlFlowNodeContent::This => {
          self
            .def
            .push(Some(vec!["This"].into_iter().collect::<HashSet<&'a str>>()));

          self.r#use.push(None);
        }
        ControlFlowNodeContent::ClassVariableDeclaration(ref c) => {
          self.def.push(Some(
            c.identifiers
              .iter()
              .map(|ident| -> &'a str { return &ident.name })
              .collect::<HashSet<&'a str>>(),
          ));

          self.r#use.push(None);
        }
        ControlFlowNodeContent::SubroutineDeclarationParam(ref p) => {
          self.def.push(Some(
            vec![&p.identifier.name as &'a str]
              .into_iter()
              .collect::<HashSet<&'a str>>(),
          ));

          self.r#use.push(None);
        }
        ControlFlowNodeContent::Statement(Statement::VariableDeclaration(
          variable_declaration,
        )) => {
          self.def.push(Some(
            variable_declaration
              .identifiers
              .iter()
              .map(|ident| -> &'a str { return &ident.name })
              .collect::<HashSet<&'a str>>(),
          ));

          self.r#use.push(None);
        }
        ControlFlowNodeContent::Statement(Statement::VariableAssignment(
          variable_assignment,
        )) => {
          self.def.push(Some(
            vec![&variable_assignment.identifier.name as &'a str]
              .into_iter()
              .collect::<HashSet<&'a str>>(),
          ));

          self
            .r#use
            .push(Self::_extract_uses_from_expr(&variable_assignment.value));
        }
        ControlFlowNodeContent::Statement(Statement::SubroutineCall(
          subroutine_call,
        )) => {
          self.def.push(None);

          let mut set = HashSet::new();

          for param in &subroutine_call.params {
            if let Some(uses) = Self::_extract_uses_from_expr(param) {
              set.extend(uses);
            }
          }

          if subroutine_call.has_class_specifier
            && cf_graph
              .idents
              .contains(&subroutine_call.class_var_ref.name as &'a str)
          {
            set.insert(&subroutine_call.class_var_ref.name);
          }

          if !set.is_empty() {
            self.r#use.push(Some(set));
          } else {
            self.r#use.push(None);
          }
        }
        _ => {}
      }
    }
  }

  fn _extract_uses_from_expr(
    expr: &'a ExpressionInfo,
  ) -> Option<HashSet<&'a str>> {
    match expr.expr {
      Expression::Primary(ref p) => match p {
        Primary::This(_) => {
          return Some(vec!["This"].into_iter().collect::<HashSet<&'a str>>());
        }
        Primary::Identifier(ident) => {
          return Some(
            vec![&ident.name as &'a str]
              .into_iter()
              .collect::<HashSet<&'a str>>(),
          );
        }
        _ => {
          return None;
        }
      },
      Expression::Unary { ref rhs, .. } => {
        return Self::_extract_uses_from_expr(rhs);
      }
      Expression::BinOp {
        ref lhs, ref rhs, ..
      } => {
        let mut set = Self::_extract_uses_from_expr(lhs);

        if let Some(rhs_set) = Self::_extract_uses_from_expr(rhs) {
          if let Some(ref mut set) = set {
            set.extend(rhs_set);
          } else {
            set = Some(rhs_set);
          }
        }

        return set;
      }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////
// Controlflow Graph
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
enum ControlFlowNodeContent<'a> {
  ClassVariableDeclaration(ClassVariableDeclaration),
  SubroutineDeclarationParam(SubroutineDeclarationParam),
  Statement(&'a Statement),
  This,
}

impl<'a> From<ClassVariableDeclaration> for ControlFlowNodeContent<'a> {
  fn from(value: ClassVariableDeclaration) -> Self {
    return Self::ClassVariableDeclaration(value);
  }
}

impl<'a> From<SubroutineDeclarationParam> for ControlFlowNodeContent<'a> {
  fn from(value: SubroutineDeclarationParam) -> Self {
    return Self::SubroutineDeclarationParam(value);
  }
}

impl<'a> From<&'a Statement> for ControlFlowNodeContent<'a> {
  fn from(value: &'a Statement) -> Self {
    return Self::Statement(value);
  }
}

#[derive(Debug)]
struct ControlFlowNode<'a> {
  content: ControlFlowNodeContent<'a>,
  successors: Vec<usize>,
}

impl<'a> ControlFlowNode<'a> {
  fn new(content: ControlFlowNodeContent<'a>, successors: Vec<usize>) -> Self {
    return Self {
      content,
      successors,
    };
  }
}

#[derive(Debug)]
struct ControlFlowGraph<'a> {
  idents: HashSet<&'a str>,
  nodes: Vec<ControlFlowNode<'a>>,
  index: Cell<usize>,
}

impl<'a> Default for ControlFlowGraph<'a> {
  fn default() -> Self {
    return Self::new(Cell::new(0), vec![]);
  }
}

impl<'a> ControlFlowGraph<'a> {
  fn new(index: Cell<usize>, nodes: Vec<ControlFlowNode<'a>>) -> Self {
    return Self {
      idents: HashSet::new(),
      index,
      nodes,
    };
  }

  fn get_index(&self) -> usize {
    return self.index.get();
  }

  fn increase_index(&self) -> usize {
    let current = self.index.get();
    return self.index.replace(current + 1);
  }

  fn _nodes_from_class_vars(
    &mut self,
    class: &'a ClassDeclaration,
    subroutine: &'a SubroutineDeclaration,
  ) -> Vec<ControlFlowNode<'a>> {
    let mut nodes = vec![];

    for dec in class.class_vars.iter() {
      if subroutine.subroutine_type == Keyword::Function
        && dec.var_type == SymbolKind::Field
      {
        continue;
      }

      self
        .idents
        .extend(dec.identifiers.iter().map(|i| -> &'a str {
          return &i.name;
        }));

      nodes.push(ControlFlowNode::new(
        dec.clone().into(),
        vec![self.increase_index() + 1],
      ));
    }

    return nodes;
  }

  fn _nodes_from_subroutine_params(
    &mut self,
    subroutine: &'a SubroutineDeclaration,
  ) -> Vec<ControlFlowNode<'a>> {
    let mut nodes = vec![];

    if subroutine.subroutine_type != Keyword::Function {
      self.idents.insert("This");

      nodes.push(ControlFlowNode::new(
        ControlFlowNodeContent::This,
        vec![self.increase_index() + 1],
      ));
    }

    for param in subroutine.params.iter() {
      self.idents.insert(&param.identifier.name);

      nodes.push(ControlFlowNode::new(
        param.clone().into(),
        vec![self.increase_index() + 1],
      ));
    }

    return nodes;
  }

  fn _nodes_from_block(
    &mut self,
    block: &'a Block,
    successor: Option<usize>,
  ) -> Vec<ControlFlowNode<'a>> {
    let mut nodes = vec![];
    let statement_length = block.statements.len();

    for (idx, statement) in block.statements.iter().enumerate() {
      match statement {
        Statement::VariableDeclaration(v) => {
          self.idents.extend(
            v.identifiers.iter().map(|i| -> &'a str { return &i.name }),
          );
        }
        Statement::VariableAssignment(v) => {
          self.idents.insert(&v.identifier.name);
        }
        _ => {}
      }

      match statement {
        Statement::VariableDeclaration(_)
        | Statement::VariableAssignment(_)
        | Statement::SubroutineCall(_) => {
          let index = self.increase_index();
          nodes.push(ControlFlowNode::new(statement.into(), vec![index + 1]));
        }
        Statement::Return(_) => {
          nodes.push(ControlFlowNode::new(statement.into(), vec![usize::MAX]));
        }
        Statement::If(if_statement) => {
          let if_block_index = self.increase_index();
          let mut if_block_nodes =
            Self::_nodes_from_block(self, &if_statement.if_body, successor);
          let else_block_index = self.get_index();
          let if_statement_node = ControlFlowNode::new(
            statement.into(),
            vec![if_block_index + 1, else_block_index],
          );
          let else_block_nodes =
            if let Some(else_body) = &if_statement.else_body {
              let else_statement_nodes =
                Self::_nodes_from_block(self, else_body, successor);

              if let Some(node) = if_block_nodes.last_mut() {
                node.successors = vec![self.get_index()];
              }

              else_statement_nodes
            } else {
              vec![]
            };

          nodes.push(if_statement_node);
          nodes.extend(if_block_nodes);
          nodes.extend(else_block_nodes);
        }
        Statement::While(while_statement) => {
          let while_block_index = self.increase_index();
          let mut while_block_nodes = Self::_nodes_from_block(
            self,
            &while_statement.body,
            Some(while_block_index),
          );
          if let Some(node) = while_block_nodes.last_mut() {
            // FIXME: This breaks in nested loops because it overrides
            // the successor of the last node in the while block
            node.successors = vec![while_block_index];
          }

          let succ = if let Some(successor) = successor {
            successor
          } else {
            self.get_index()
          };

          let while_statement_node = ControlFlowNode::new(
            statement.into(),
            vec![
              while_block_index + 1,
              if statement_length - 1 == idx {
                succ
              } else {
                self.get_index()
              },
            ],
          );

          nodes.push(while_statement_node);
          nodes.extend(while_block_nodes);
        }
      }
    }

    return nodes;
  }
}

impl<'a> From<(&'a ClassDeclaration, &'a SubroutineDeclaration)>
  for ControlFlowGraph<'a>
{
  fn from(value: (&'a ClassDeclaration, &'a SubroutineDeclaration)) -> Self {
    let (class, subroutine) = value;
    let mut cf_graph = ControlFlowGraph::default();
    let mut nodes = Vec::new();

    nodes.extend(cf_graph._nodes_from_class_vars(class, subroutine));
    nodes.extend(cf_graph._nodes_from_subroutine_params(subroutine));
    nodes.extend(cf_graph._nodes_from_block(&subroutine.body, None));

    cf_graph.nodes = nodes;

    return cf_graph;
  }
}

////////////////////////////////////////////////////////////////////////////////
// Controlflow Analysis
////////////////////////////////////////////////////////////////////////////////

pub(crate) struct ControlflowAnalyser<'a> {
  cf_graph: HashMap<String, ControlFlowGraph<'a>>,
  liveness: Option<Liveness<'a>>,
}

impl<'a> ControlflowAnalyser<'a> {
  fn new(
    cf_graph: HashMap<String, ControlFlowGraph<'a>>,
    liveness: Option<Liveness<'a>>,
  ) -> Self {
    return Self { cf_graph, liveness };
  }

  //////////////////////////////////////////////////////////////////////////////
  // Control Flow Graph
  //////////////////////////////////////////////////////////////////////////////

  pub(crate) fn factory(program: &'a [Program]) -> Self {
    let subroutines: HashMap<String, ControlFlowGraph<'a>> = program
      .iter()
      .flat_map(|p| return &p.classes)
      .flat_map(|c| {
        return c
          .subroutines
          .iter()
          .map(|s| {
            return (
              format!("{}.{}", s.class.name, s.identifier.name),
              (c, s).into(),
            );
          })
          .collect::<Vec<(String, ControlFlowGraph<'a>)>>();
      })
      .collect();

    return Self::new(subroutines, None);
  }

  pub(crate) fn print_control_flow_graph(&self) {
    let mut sorted_subroutines: Vec<(&String, &ControlFlowGraph)> =
      self.cf_graph.iter().collect();
    sorted_subroutines.sort_by(|&a, &b| return a.0.cmp(b.0));

    let mut dot_prog = String::from("digraph ControlFlowGraph {\n");
    let mut current_class = String::new();
    let mut class_index = 0;
    let mut global_counter = 0;

    for (sub_index, (name, cf_graph)) in sorted_subroutines.iter().enumerate() {
      let (class_name, subroutine_name) =
        name.split_once('.').expect(error_fmt_src!(
        "(ControlFlowGraphError) Subroutine name is not in the correct format!"
      ));

      if current_class != class_name {
        if !current_class.is_empty() {
          dot_prog.push_str("    }\n")
        }

        current_class = class_name.to_string();
        dot_prog.push_str(&format!(
          "  subgraph \"cluster_{class_index}\" {{\n    label=\"{class_name}\";\n"
        ));
        class_index += 1;
      }

      let mut dot_sub = format!(
        "    subgraph \"cluster_{class_index}_{sub_index}\" \
        {{\n      label=\"{subroutine_name}\";\n"
      );
      let mut realative_counter = 0;
      let mut class_node = false;

      for (node_index, node) in cf_graph.nodes.iter().enumerate() {
        let statement_name = match node.content {
          ControlFlowNodeContent::This => String::from("This"),
          ControlFlowNodeContent::ClassVariableDeclaration(ref c) => {
            format!(
              "({}) ClassVariableDeclaration -> {}",
              c.var_type,
              c.identifiers
                .iter()
                .map(|i| return if i.is_class_var {
                  format!("(class variable) {}.{}", i.scope.class_name, i.name)
                } else {
                  i.name.clone()
                })
                .collect::<Vec<String>>()
                .join(", ")
            )
          }
          ControlFlowNodeContent::SubroutineDeclarationParam(ref p) => {
            if class_node {
              dot_sub.push_str("    node [style=outline]");
              class_node = false;
            }

            format!("SubroutineDeclarationParam -> {}", p.identifier.name)
          }
          ControlFlowNodeContent::Statement(
            Statement::VariableDeclaration(s),
          ) => {
            class_node = false;

            format!(
              "VariableDeclaration -> {}",
              s.identifiers
                .iter()
                .map(|i| return i.name.clone())
                .collect::<Vec<String>>()
                .join(", ")
            )
          }
          ControlFlowNodeContent::Statement(Statement::VariableAssignment(
            s,
          )) => {
            format!(
              "VariableAssignment -> {}",
              if s.identifier.is_class_var {
                format!(
                  "(class variable) {}.{}",
                  s.identifier.scope.class_name, s.identifier.name
                )
              } else {
                s.identifier.name.clone()
              }
            )
          }
          ControlFlowNodeContent::Statement(Statement::SubroutineCall(s)) => {
            format!(
              "SubroutineCall -> {}.{}",
              s.class_var_ref.name, s.identifier.name
            )
          }
          ControlFlowNodeContent::Statement(Statement::Return(_)) => {
            String::from("Return")
          }
          ControlFlowNodeContent::Statement(Statement::If(_)) => {
            String::from("If")
          }
          ControlFlowNodeContent::Statement(Statement::While(_)) => {
            String::from("While")
          }
        };

        for successor in &node.successors {
          dot_sub.push_str(&format!(
            "      {} [label=\"{}\", shape=box];\n",
            global_counter + node_index,
            statement_name
          ));

          if *successor != usize::MAX {
            dot_sub.push_str(&format!(
              "      {} -> {}\n",
              global_counter + node_index,
              global_counter + successor
            ));
          }

          realative_counter += 1;
        }
      }

      dot_sub.push_str("    }\n");
      dot_prog.push_str(&dot_sub);

      global_counter += realative_counter;
    }

    dot_prog.push_str("  }\n}");

    info_print!("Control Flow Graph (Graphviz):\n{}", dot_prog);
  }

  //////////////////////////////////////////////////////////////////////////////
  // Liveness Analysis
  //////////////////////////////////////////////////////////////////////////////

  pub(crate) fn liveness_analysis(&'a mut self) -> HashMap<&'a str, usize> {
    let mut liveness = Liveness::default();
    let min_register_count = liveness.get_min_register_count(&self.cf_graph);

    self.liveness = Some(liveness);

    return min_register_count;
  }
}
