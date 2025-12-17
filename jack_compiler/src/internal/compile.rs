////////////////////////////////////////////////////////////////////////////////
// File: src/internal/compile.rs
// Description: Compilation module
//
// Author: Leon Heidelbach <leon.heidelbach@hhu.de>
// Date: 15.05.2024
//
// License: GPLv3
////////////////////////////////////////////////////////////////////////////////

use std::{collections::BTreeMap, path::Path};

use crate::internal::{
  controlflow::ControlflowAnalyser,
  intermediate::{GeneratorInfo, IntermediateRepresentation},
  parse::JackParser,
  xml::{XmlSerializableProgram, XmlSerializableTokenStream},
};

use super::{
  ast::Program,
  check::{GlobalScope, TypeCheckableProgram, TypeChecker},
};

use shared::{
  error_panic, error_panic_src, green, info_print,
  io::{
    cli::{CommandLineParser, Token},
    os::{dir_check, generate_output, out_dir_check, read_file_list, FileInfo},
  },
  red,
  util::{parse::parse_programs, settings::Setting},
  yellow,
};

////////////////////////////////////////////////////////////////////////////////
// Constants
////////////////////////////////////////////////////////////////////////////////

pub(crate) const MAIN_ENTRY_POINT: &str = "Main.main";
const JACK_EXTENSION: &str = "jack";
const VM_EXTENSION: &str = "vm";
const XML_EXTENSION: &str = "xml";

////////////////////////////////////////////////////////////////////////////////
// Typecheck Step
////////////////////////////////////////////////////////////////////////////////

pub(crate) fn typecheck_parse_trees(
  parse_trees: &[Program],
  files: &[FileInfo],
) -> GlobalScope {
  let prog = parse_trees.iter().enumerate().fold(
    TypeCheckableProgram::default(),
    |mut acc, (i, tree)| {
      tree.classes.iter().for_each(|c| {
        acc.insert_class_declaration(c, &files[i]);
      });

      return acc;
    },
  );
  let mut type_checker = TypeChecker::factory(&prog);
  let result = type_checker.typecheck();
  let type_map = if let Ok(result) = result {
    result
  } else {
    println!("{}", result.unwrap_err());
    error_panic!(
      "(TypecheckError) Typechecking found errors. {}!",
      red!("Aborting compilation", true, false)
    );
  };

  if Setting::PrintAll.is_set().is_ok() || Setting::PrintTypes.is_set().is_ok()
  {
    info_print!("Type Map: {:#?}", type_map);
  }

  return type_map.clone();
}

////////////////////////////////////////////////////////////////////////////////
// XML Generation Step
////////////////////////////////////////////////////////////////////////////////

fn generate_xml_files(
  programs: Vec<Program>,
  files: Vec<FileInfo>,
  output_path: Option<&Path>,
) {
  info_print!("Generating XML Token files");

  let tokens: &Vec<XmlSerializableTokenStream> = &programs
    .iter()
    .map(XmlSerializableTokenStream::from)
    .collect();

  let token_files = &files
    .clone()
    .into_iter()
    .map(|mut f| {
      f.set_file_name(&format!("{}T", f.stem), XML_EXTENSION);
      return f;
    })
    .collect::<Vec<FileInfo>>();

  generate_output(tokens, token_files, dir_check(output_path), XML_EXTENSION);

  info_print!("Generating XML AST files");

  let programs: &Vec<XmlSerializableProgram> = &programs
    .into_iter()
    .map(XmlSerializableProgram::from)
    .collect();

  generate_output(programs, &files, dir_check(output_path), XML_EXTENSION);

  info_print!("{}!", green!("XML generation successful", true, false));
}

////////////////////////////////////////////////////////////////////////////////
// Liveness Analysis Step
////////////////////////////////////////////////////////////////////////////////

fn liveness_anaylsis<'a>(
  cfa: &'a mut ControlflowAnalyser<'a>,
  programs: &[Program],
  files: &[FileInfo],
) {
  let _ = typecheck_parse_trees(programs, files);
  let liveness = cfa
    .liveness_analysis()
    .into_iter()
    .collect::<BTreeMap<_, _>>();

  info_print!("{}", green!("Liveness analysis successful", true, false));

  for subroutine in liveness.iter() {
    let (name, min_registers) = subroutine;
    println!(
      "  {}: {}",
      green!(name, false, false),
      yellow!(&min_registers.to_string(), false, false)
    );
  }
}

////////////////////////////////////////////////////////////////////////////////
// VM Generation Step
////////////////////////////////////////////////////////////////////////////////

fn generate_vm_files(mut programs: Vec<Program>, files: Vec<FileInfo>) {
  let type_map = typecheck_parse_trees(&programs, &files);

  if !type_map.map.contains_key(MAIN_ENTRY_POINT) {
    error_panic!(
      "(TypecheckError) Missing entry point. The program does not contain a \
        `Main` class with a `main` function!"
    );
  }

  info_print!("{}", green!("Typechecking successful", true, false));
  info_print!("Generating VM files");

  programs.iter_mut().enumerate().for_each(|(i, p)| {
    p.instructions
      .extend(p.intermediate(&GeneratorInfo::new(&files[i], &type_map)))
  });

  generate_output(&programs, &files, out_dir_check().as_deref(), VM_EXTENSION);

  info_print!("{}", green!("Compilation successful!", true, false));
}

////////////////////////////////////////////////////////////////////////////////
// Compilation Step
////////////////////////////////////////////////////////////////////////////////

pub(crate) fn compile(_c: &mut CommandLineParser, t: &mut Token) {
  let files: Vec<FileInfo> = read_file_list(
    &t.parameters.clone().unwrap_or_else(|| {
      error_panic_src!("(ArgError) No input files provided!")
    }),
    JACK_EXTENSION,
  );

  let programs = parse_programs(&files, JackParser::parse);
  info_print!("{}", green!("Parsing successful", true, false));

  let run_liveness = Setting::Liveness.is_set().is_ok();
  let print_cfg = Setting::PrintControlFlowGraph.is_set().is_ok();
  let print_all = Setting::PrintAll.is_set().is_ok();

  if print_all || run_liveness || print_cfg {
    let mut cfa = ControlflowAnalyser::factory(&programs);

    if print_all || print_cfg {
      cfa.print_control_flow_graph();
    }

    if run_liveness {
      liveness_anaylsis(&mut cfa, &programs, &files);
      return;
    }
  }

  if let Ok(Some(result)) = Setting::Xml.is_set() {
    generate_xml_files(programs, files, result.first().map(Path::new));
  } else {
    generate_vm_files(programs, files);
  }
}
