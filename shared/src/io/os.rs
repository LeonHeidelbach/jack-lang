////////////////////////////////////////////////////////////////////////////////
// File: src/io/os.rs
// Description: OS interaction module
//
// Author: Leon Heidelbach <leon.heidelbach@hhu.de>
// Date: 01.05.2024
//
// License: GPLv3
////////////////////////////////////////////////////////////////////////////////

use std::ffi::OsStr;
use std::path::Path;
use std::{fs, path::PathBuf};

use crate::util::traits::Serializable;
use crate::{error_fmt, error_panic, info_print, util::settings::Setting};

////////////////////////////////////////////////////////////////////////////////
// Filesystem Interactions
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub struct FileInfo {
  pub name: String,
  pub stem: String,
  pub path: PathBuf,
  pub content: String,
}

impl FileInfo {
  pub fn new(name: String, path: PathBuf, content: String) -> Self {
    return Self {
      name,
      stem: path
        .file_stem()
        .expect(error_fmt!("(ArgError) No file stem provided!"))
        .to_str()
        .expect(error_fmt!("(ArgError) Invalid path"))
        .to_string(),
      path,
      content,
    };
  }

  pub fn from_path(path: PathBuf, file_extension: Option<&str>) -> Self {
    let path_str = path
      .to_str()
      .expect(error_fmt!("(ArgError) Invalid path"))
      .to_string();

    if let Some(file_extension) = file_extension {
      if file_extension
        != path
          .extension()
          .expect(error_fmt!("(ArgError) No file extension provided!"))
      {
        error_panic!(
          "(ArgError) Invalid file extension on file: `{}`. Expected extension `.{}`",
          path_str,
          file_extension
        );
      }
    }

    return Self::new(
      path_str.clone(),
      path,
      fs::read_to_string(&path_str)
        .unwrap_or_else(|e| error_panic!("(IOError) [{}] {}", &path_str, e))
        .replace('\t', "    ")
        .replace("\r\n", "\n"), // INFO: This might have performance implications
    );
  }

  pub fn set_file_name(&mut self, name: &str, extension: &str) {
    self.name = name.to_string();
    self.path.set_file_name(name);
    self.path.set_extension(extension);
    self.stem = self
      .path
      .file_stem()
      .expect(error_fmt!("(ArgError) No file stem provided!"))
      .to_str()
      .expect(error_fmt!("(ArgError) Invalid path"))
      .to_string();
  }
}

pub fn read_file_list(paths: &[String], file_extension: &str) -> Vec<FileInfo> {
  let mut file_info_list = Vec::new();
  let deduped_paths = paths
    .iter()
    .cloned()
    .collect::<std::collections::HashSet<String>>();

  for x in deduped_paths.iter() {
    let path = PathBuf::from(x);

    if path.is_dir() {
      let dir_entries = fs::read_dir(&path).unwrap_or_else(|e| {
        error_panic!("(IOError) [{}] {}", &path.to_str().unwrap(), e)
      });

      for entry in dir_entries {
        let entry = entry.unwrap_or_else(|e| {
          error_panic!("(IOError) Failed to read directory entry: {}", e)
        });
        let entry_path = entry.path();

        if entry_path.is_file()
          && entry_path.extension() == Some(OsStr::new(file_extension))
        {
          file_info_list.push(FileInfo::from_path(entry_path, None));
        }
      }
    } else {
      file_info_list.push(FileInfo::from_path(path, Some(file_extension)));
    }
  }

  return file_info_list;
}

pub fn dir_check(path: Option<&Path>) -> Option<&Path> {
  if let Some(path) = path {
    if !path.is_dir() {
      std::fs::create_dir_all(path)
        .expect(error_fmt!("(IOError) Failed to create output directory"));
      info_print!(
        "Created output directory: `{}`",
        path.to_str().expect(error_fmt!("(ArgError) Invalid path"))
      );
    }
    return Some(path);
  } else {
    return None;
  }
}

pub fn out_dir_check() -> Option<PathBuf> {
  return if let Ok(Some(values)) = Setting::Output.is_set() {
    let output_dir = PathBuf::from(
      values
        .first()
        .expect(error_fmt!("(ArgError) No output directory provided!")),
    );

    dir_check(Some(output_dir.as_path()));

    Some(output_dir)
  } else {
    None
  };
}

pub fn write_file(file_path: PathBuf, content: String) -> (PathBuf, String) {
  std::fs::write(&file_path, &content).expect(error_fmt!(
    "(IOError) Failed to write to file: `{}`",
    &file_path
      .to_str()
      .expect(error_fmt!("(ArgError) Invalid path"))
  ));

  return (file_path, content);
}

pub fn generate_output(
  programs: &[impl Serializable<Output = String>],
  files: &[FileInfo],
  output_dir: Option<&Path>,
  output_file_ext: &str,
) -> Vec<(PathBuf, String)> {
  return programs
    .iter()
    .enumerate()
    .map(|(index, program)| {
      let output = program.serialize();
      let mut file_path = files[index].path.clone();
      file_path.set_extension(output_file_ext);

      if let Some(output_dir) = output_dir {
        let mut output_dir = output_dir.to_path_buf().clone();
        output_dir.push(
          file_path
            .file_name()
            .expect(error_fmt!("(ArgError) No file name provided!")),
        );

        file_path = output_dir;
      }

      return write_file(file_path, output);
    })
    .collect();
}
