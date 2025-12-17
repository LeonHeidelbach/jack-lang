////////////////////////////////////////////////////////////////////////////////
// File: src/internal/serialize.rs
// Description: Serialization module
//
// Author: Leon Heidelbach <leon.heidelbach@hhu.de>
// Date: 01.05.2024
//
// License: GPLv3
////////////////////////////////////////////////////////////////////////////////

use super::{
  asm::{AToken, CCompToken, CDestinationToken, CJumpToken, CToken, Token},
  parse::Program,
};

use shared::util::{helpers::U16Manipulation, traits::Serializable};

////////////////////////////////////////////////////////////////////////////////
// Program Serialization
////////////////////////////////////////////////////////////////////////////////

impl Serializable for Program {
  type Output = String;
  fn serialize(&self) -> Self::Output {
    return self.tokens.iter().fold(String::new(), |acc, token| {
      if let Some(serialized) = token.serialize() {
        return acc + &serialized + "\n";
      }

      return acc;
    });
  }
}

////////////////////////////////////////////////////////////////////////////////
// Token Serialization
////////////////////////////////////////////////////////////////////////////////

impl Serializable for Token {
  type Output = Option<String>;
  fn serialize(&self) -> Self::Output {
    return match self {
      Token::A(token) => Some(token.serialize().to_binary_string()),
      Token::C(token) => Some(token.serialize().to_binary_string()),
      Token::Label => None,
    };
  }
}

impl Serializable for AToken {
  type Output = u16;
  fn serialize(&self) -> Self::Output {
    return match self {
      AToken::Literal(literal) => *literal,
      AToken::Mnemonic(mnemonic) => mnemonic.get(),
    };
  }
}

impl Serializable for CToken {
  type Output = u16;
  fn serialize(&self) -> Self::Output {
    return 0b111 << 13
      | self.cmp.serialize()
      | self.dst.serialize()
      | self.jmp.serialize();
  }
}

impl Serializable for CCompToken {
  type Output = u16;
  fn serialize(&self) -> Self::Output {
    return match self {
      CCompToken::ARegister => 0b0110000 << 6,
      CCompToken::ARegisterMinusDRegister => 0b0000111 << 6,
      CCompToken::ARegisterMinusOne => 0b0110010 << 6,
      CCompToken::ARegisterPlusOne => 0b0110111 << 6,
      CCompToken::DRegister => 0b0001100 << 6,
      CCompToken::DRegisterAndARegister => 0,
      CCompToken::DRegisterAndRAM => 0b1000000 << 6,
      CCompToken::DRegisterMinusARegister => 0b0010011 << 6,
      CCompToken::DRegisterMinusOne => 0b0001110 << 6,
      CCompToken::DRegisterMinusRAM => 0b1010011 << 6,
      CCompToken::DRegisterOrARegister => 0b0010101 << 6,
      CCompToken::DRegisterOrRAM => 0b1010101 << 6,
      CCompToken::DRegisterPlusARegister => 0b0000010 << 6,
      CCompToken::DRegisterPlusOne => 0b0011111 << 6,
      CCompToken::DRegisterPlusRAM => 0b1000010 << 6,
      CCompToken::MinusARegister => 0b0110011 << 6,
      CCompToken::MinusDRegister => 0b0001111 << 6,
      CCompToken::MinusOne => 0b0111010 << 6,
      CCompToken::MinusRAM => 0b1110011 << 6,
      CCompToken::NotARegister => 0b0110001 << 6,
      CCompToken::NotDRegister => 0b0001101 << 6,
      CCompToken::NotRAM => 0b1110001 << 6,
      CCompToken::One => 0b0111111 << 6,
      CCompToken::RAMMinusDRegister => 0b1000111 << 6,
      CCompToken::RAMMinusOne => 0b1110010 << 6,
      CCompToken::RAMPlusOne => 0b1110111 << 6,
      CCompToken::RAM => 0b1110000 << 6,
      CCompToken::Zero => 0b0101010 << 6,
    };
  }
}

impl Serializable for CDestinationToken {
  type Output = u16;
  fn serialize(&self) -> Self::Output {
    return ((self.a_reg as u8) << 5) as u16
      | ((self.d_reg as u8) << 4) as u16
      | ((self.ram as u8) << 3) as u16;
  }
}

impl Serializable for CJumpToken {
  type Output = u16;
  fn serialize(&self) -> Self::Output {
    return ((self.lt as u8) << 2) as u16
      | ((self.eq as u8) << 1) as u16
      | (self.gt as u8) as u16;
  }
}
