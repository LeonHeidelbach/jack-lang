////////////////////////////////////////////////////////////////////////////////
// File: src/internal/serialize.rs
// Description: Serialization module
//
// Author: Leon Heidelbach <leon.heidelbach@hhu.de>
// Date: 15.05.2024
//
// License: GPLv3
////////////////////////////////////////////////////////////////////////////////

use super::{
  ast::Program,
  xml::{
    OwnedXmlSerializableTokenStream, XmlSerializableProgram,
    XmlSerializableTokenStream, XmlSerialize,
  },
};

use shared::util::traits::Serializable;

////////////////////////////////////////////////////////////////////////////////
// XML Serialization
////////////////////////////////////////////////////////////////////////////////

impl Serializable for OwnedXmlSerializableTokenStream {
  type Output = String;
  fn serialize(&self) -> String {
    return self.tokens.as_slice().to_xml(0);
  }
}

impl<'a> Serializable for XmlSerializableTokenStream<'a> {
  type Output = String;
  fn serialize(&self) -> String {
    return self.tokens.as_slice().to_xml(0);
  }
}

impl Serializable for XmlSerializableProgram {
  type Output = String;
  fn serialize(&self) -> String {
    return self.program.to_xml(0);
  }
}

////////////////////////////////////////////////////////////////////////////////
// Program Serialization
////////////////////////////////////////////////////////////////////////////////

impl Serializable for Program {
  type Output = String;
  fn serialize(&self) -> String {
    return self
      .instructions
      .iter()
      .map(|instruction| return instruction.to_string())
      .collect::<Vec<String>>()
      .join("\n");
  }
}
