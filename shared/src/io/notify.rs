////////////////////////////////////////////////////////////////////////////////
// File: src/io/notify.rs
// Description: Notification macros
//
// Author: Leon Heidelbach <leon.heidelbach@hhu.de>
// Date: 01.05.2024
//
// License: GPLv3
////////////////////////////////////////////////////////////////////////////////

use core::fmt;
use std::{
  env,
  fmt::{Display, Formatter},
  time::{SystemTime, UNIX_EPOCH},
};

use owo_colors::{OwoColorize, Stream::Stdout};

////////////////////////////////////////////////////////////////////////////////
// Constants
////////////////////////////////////////////////////////////////////////////////

pub const LOG_LEVEL_ERROR: &str = "Error";
pub const LOG_LEVEL_WARNING: &str = "Warning";
pub const LOG_LEVEL_INFO: &str = "Info";

pub const COLOR_RED: usize = 0;
pub const COLOR_YELLOW: usize = 1;
pub const COLOR_GREEN: usize = 2;
pub const COLOR_CYAN: usize = 3;

////////////////////////////////////////////////////////////////////////////////
// Color Formatting
////////////////////////////////////////////////////////////////////////////////

#[macro_export]
macro_rules! red {
  ($input:expr, $bold:expr, $underline:expr) => {
    $crate::io::notify::color_fmt(
      $input,
      $crate::io::notify::COLOR_RED,
      $bold,
      $underline,
    )
  };
}

#[macro_export]
macro_rules! yellow {
  ($input:expr, $bold:expr, $underline:expr) => {
    $crate::io::notify::color_fmt(
      $input,
      $crate::io::notify::COLOR_YELLOW,
      $bold,
      $underline,
    )
  };
}

#[macro_export]
macro_rules! green {
  ($input:expr, $bold:expr, $underline:expr) => {
    $crate::io::notify::color_fmt(
      $input,
      $crate::io::notify::COLOR_GREEN,
      $bold,
      $underline,
    )
  };
}

#[macro_export]
macro_rules! cyan {
  ($input:expr, $bold:expr, $underline:expr) => {
    $crate::io::notify::color_fmt(
      $input,
      $crate::io::notify::COLOR_CYAN,
      $bold,
      $underline,
    )
  };
}

////////////////////////////////////////////////////////////////////////////////
// General Message Formatting
////////////////////////////////////////////////////////////////////////////////

#[macro_export]
macro_rules! message_fmt_src {
  ($severity_symbol:expr, $message_type:expr, $message:tt $(,$arg:expr)*) => {
    &format!(
      "[{}] [{}] ({}::{}:{}): [{}]: {}",
      $severity_symbol,
      $crate::io::notify::generate_timestamp($crate::io::notify::GMT_OFFSET),
      file!(),
      line!(),
      column!(),
      $message_type,
      format_args!($message $(,$arg)*)
    )
  };
}

#[macro_export]
macro_rules! message_fmt {
  ($severity_symbol:expr, $message_type:expr, $message:tt $(,$arg:expr)*) => {
    &format!(
      "[{}] [{}] [{}]: {}",
      $severity_symbol,
      $crate::io::notify::generate_timestamp($crate::io::notify::GMT_OFFSET),
      $message_type,
      format_args!($message $(,$arg)*)
    )
  };
}

#[macro_export]
macro_rules! message_fmt_file {
  ($severity_symbol:expr, $message_type:expr, $vis:expr, $message:expr) => {
    &format!(
      "[{}] [{}] [{}]: {}\n{}\n",
      $severity_symbol,
      $crate::io::notify::generate_timestamp($crate::io::notify::GMT_OFFSET),
      $message_type,
      $message,
      $vis
    )
  };
}

////////////////////////////////////////////////////////////////////////////////
// Error Formatting
////////////////////////////////////////////////////////////////////////////////

#[macro_export]
macro_rules! error_fmt {
  ($message:tt $(,$arg:expr)*) => {
    $crate::message_fmt![
      $crate::red!["!", true, false],
      $crate::red![$crate::io::notify::LOG_LEVEL_ERROR, true, true],
      $message $(,$arg)*
    ]
  };
}

#[macro_export]
macro_rules! error_fmt_file {
  ($file_name:expr, $file_content:expr, $pos:expr, $message:tt,
    $error_text:tt $(,$arg:expr)*) => {
    $crate::message_fmt_file![
      $crate::red!["!", true, false],
      $crate::red![$crate::io::notify::LOG_LEVEL_ERROR, true, true],
      ($crate::io::notify::file_pos_visualization($file_name, $file_content,
        $pos, &format!($error_text $(,$arg)*))),
      $message
    ]
  };
}

#[macro_export]
macro_rules! error_fmt_src {
  ($message:tt $(,$arg:expr)*) => {
    $crate::message_fmt_src![
      $crate::red!["!", true, false],
      $crate::red![$crate::io::notify::LOG_LEVEL_ERROR, true, true],
      $message $(,$arg)*
    ]
  };
}

#[macro_export]
macro_rules! error_println {
  ($message:tt $(,$arg:expr)*) => {
    eprintln!("{}", $crate::error_fmt!($message $(,$arg)*))
  };
}

#[macro_export]
macro_rules! error_println_src {
  ($message:tt $(,$arg:expr)*) => {
    eprintln!("{}", $crate::error_fmt_src!($message $(,$arg)*))
  };
}

#[macro_export]
macro_rules! error_panic {
  ($message:tt $(,$arg:expr)*) => {
    panic!("{}", $crate::error_fmt!($message $(,$arg)*))
  };
}

#[macro_export]
macro_rules! error_panic_src {
  ($message:tt $(,$arg:expr)*) => {
    panic!("{}", $crate::error_fmt_src!($message $(,$arg)*))
  };
}

#[macro_export]
macro_rules! error_panic_fmt_file {
  ($file_name:expr, $file_content:expr, $pos:expr, $message:tt,
    $error_text:tt $(,$arg:expr)*) => {
    panic!("{}", $crate::error_fmt_file!($file_name, $file_content, $pos,
      $message, $error_text $(,$arg)*))
  };
}

#[macro_export]
macro_rules! error_unreachable {
  ($message:tt $(,$arg:expr)*) => {
    unreachable!("\n{}", $crate::error_fmt_src!($message $(,$arg)*))
  };
}

////////////////////////////////////////////////////////////////////////////////
// Warning Formatting
////////////////////////////////////////////////////////////////////////////////

#[macro_export]
macro_rules! warning_fmt {
  ($message:tt $(,$arg:expr)*) => {
    $crate::message_fmt_src![
      $crate::yellow!["?", true, false],
      $crate::yellow![$crate::io::notify::LOG_LEVEL_WARNING, true, true], $message $(,$arg)*
    ]
  };
}

#[macro_export]
macro_rules! warning_fmt_file {
  ($file_name:expr, $file_content:expr, $pos:expr, $message:tt, $warn_text:tt $(,$arg:expr)*) => {
    $crate::message_fmt_file![
      $crate::yellow!["!", true, false],
      $crate::yellow![$crate::io::notify::LOG_LEVEL_WARNING, true, true],
      ($crate::io::notify::file_pos_visualization($file_name, $file_content, $pos,
        &format!($warn_text $(,$arg)*))),
      $message
    ]
  };
}

#[macro_export]
macro_rules! warning_print {
  ($message:tt $(,$arg:expr)*) => {
    println!("{}", $crate::warning_fmt!($message $(,$arg)*))
  };
}

#[macro_export]
macro_rules! warning_println_file {
  ($file_name:expr, $file_content:expr, $pos:expr, $message:tt, $warn_text:tt $(,$arg:expr)*) => {
    println!("{}",
      $crate::warning_fmt_file!($file_name, $file_content, $pos, $message, $warn_text $(,$arg)*)
    )
  };
}

////////////////////////////////////////////////////////////////////////////////
// Info Formatting
////////////////////////////////////////////////////////////////////////////////

#[macro_export]
macro_rules! info_fmt {
  ($message:tt $(,$arg:expr)*) => {
    $crate::message_fmt![
      $crate::green!["+", true, false],
      $crate::green![$crate::io::notify::LOG_LEVEL_INFO, true, true],
      $message $(,$arg)*
    ]
  };
}

#[macro_export]
macro_rules! info_print {
  ($message:tt $(,$arg:expr)*) => {
    println!("{}", $crate::info_fmt!($message $(,$arg)*))
  };
}

////////////////////////////////////////////////////////////////////////////////
// Error Handling
////////////////////////////////////////////////////////////////////////////////

pub(crate) struct Position {
  line: usize,
  line_content: String,
  column: usize,
}

impl Position {
  pub(crate) fn new(file_contents: &str, pos: usize) -> Option<Self> {
    let chars = file_contents.chars();

    if pos > chars.clone().count() {
      return None;
    }

    let mut relative_column = 0;
    let mut line = 1;
    let mut line_content = String::new();
    let mut done = false;
    let mut skip_index = false;

    for (current_pos, c) in chars.enumerate() {
      if current_pos == pos {
        done = true;
      }

      if c == '\n' {
        if done {
          break;
        }

        line += 1;
        relative_column = 0;
        skip_index = true;
        line_content.clear();
      } else {
        line_content.push(c);

        if !done && !skip_index {
          relative_column += 1;
        }

        skip_index = false;
      }
    }

    return Some(Self {
      line,
      line_content,
      column: relative_column,
    });
  }
}

pub(crate) struct Error {
  message: String,
  pos: Position,
  path: String,
}

impl Error {
  pub(crate) fn new_from_pos(message: &str, pos: Position) -> Self {
    return Self {
      message: message.to_string(),
      pos,
      path: String::new(),
    };
  }

  pub(crate) fn with_path(mut self, path: &str) -> Self {
    self.path = path.to_string();
    return self;
  }
}

impl Display for Error {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    let digits = self.pos.line.to_string().len();
    let spacing = " ".repeat(digits.max(1));
    let underline = " ".repeat(self.pos.column) + "^-----";

    return f.write_fmt(format_args!(
      "{s}{arrow} {p}:{l}:{c}\n\
       {s} {pipe}\n\
       {l} {pipe} {line}\n\
       {s} {pipe} {u}\n\
       {s} {pipe}\n\
       {s} {equals} {message}",
      s = spacing,
      p = green!(&self.path, false, false),
      l = yellow!(&self.pos.line.to_string(), true, false),
      u = red!(&underline, true, false),
      line = self.pos.line_content,
      c = red!(&(self.pos.column + 1).to_string(), false, false),
      message = self.message,
      arrow = green!("-->", true, false),
      pipe = green!("|", true, false),
      equals = green!("=", true, false)
    ));
  }
}

////////////////////////////////////////////////////////////////////////////////
// Helper Functions
////////////////////////////////////////////////////////////////////////////////

// FIXME: This should not be hardcoded. To get the proper GMT offset programatically
// without using any external crates, we might have to get the time from C.
pub const GMT_OFFSET: i64 = 2;

pub fn generate_timestamp(timezone_offset_hours: i64) -> String {
  let current_time = SystemTime::now()
    .duration_since(UNIX_EPOCH)
    .unwrap_or_else(|_| {
      error_panic_src!("(TimeError) Failed to get current time")
    })
    .as_secs() as i64;
  let offset_seconds = timezone_offset_hours * 3600;

  let local_time = current_time + offset_seconds;
  let hours = (local_time / 3600) % 24;
  let minutes = (local_time / 60) % 60;
  let seconds = local_time % 60;

  return format!("{:02}:{:02}:{:02}", hours, minutes, seconds);
}

pub fn file_pos_visualization(
  file_name: &str,
  file_contents: &str,
  pos: usize,
  message: &str,
) -> String {
  let pos = Position::new(file_contents, pos)
    .expect(error_fmt!("Invalid position in file {}", file_name));

  return Error::new_from_pos(message, pos)
    .with_path(file_name)
    .to_string();
}

////////////////////////////////////////////////////////////////////////////////
// Color Support
////////////////////////////////////////////////////////////////////////////////

lazy_static::lazy_static! {
  pub(crate) static ref RGB_COLOR_SUPPORT: bool = {
    env::var("COLORTERM")
      .map(|val| return val == "truecolor" || val == "24bit")
      .unwrap_or(false)
  };
}

pub fn color_fmt(
  input: &str,
  color: usize,
  bold: bool,
  underline: bool,
) -> String {
  let mut fmt = if *RGB_COLOR_SUPPORT {
    match color {
      COLOR_RED => input.fg_rgb::<0xFF, 0x65, 0x78>().to_string(),
      COLOR_YELLOW => input.fg_rgb::<0xEA, 0xCB, 0x64>().to_string(),
      COLOR_GREEN => input.fg_rgb::<0x9D, 0xD2, 0x74>().to_string(),
      COLOR_CYAN => input.fg_rgb::<0x6D, 0xC7, 0xE3>().to_string(),
      _ => input.to_string(),
    }
  } else {
    match color {
      COLOR_RED => input
        .if_supports_color(Stdout, |t| {
          return t.bright_red();
        })
        .to_string(),
      COLOR_YELLOW => input
        .if_supports_color(Stdout, |t| {
          return t.bright_yellow();
        })
        .to_string(),
      COLOR_GREEN => input
        .if_supports_color(Stdout, |t| {
          return t.bright_green();
        })
        .to_string(),
      COLOR_CYAN => input
        .if_supports_color(Stdout, |t| return t.bright_cyan())
        .to_string(),
      _ => input.to_string(),
    }
  };

  if bold && underline {
    fmt = fmt.bold().underline().to_string();
  } else if bold {
    fmt = fmt.bold().to_string();
  } else if underline {
    fmt = fmt.underline().to_string();
  }

  return fmt;
}
