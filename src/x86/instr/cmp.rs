use nom::*;

use crate::x86::{
    instr::{DecodeInstruction, Instruction, Opcode, REX},
    Width,
};

#[derive(Debug, PartialEq)]
crate struct Cmp {}

impl DecodeInstruction for Cmp {
    fn try_parse(input: &[u8], rex: Option<REX>) -> IResult<&[u8], Instruction> {
        alt!(input, call!(Cmp::parse_x39, rex))
    }
}

impl Cmp {
    // 39 /r            => CMP r/m16, r16
    // 39 /r            => CMP r/m32, r32
    // REX.W + 39 /r    => CMP r/m64, r64
    instr!(parse_x39, Opcode::Cmp, [0x39], r/m32, /r32);
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::x86::{
        instr::{
            Displacement, EffectiveAddress, LogicalAddress,
            Operand::{Memory as OpMem, Register as OpReg},
        },
        register::ctors::*,
    };

    #[test]
    fn instr_cmp_39() {
        assert_eq!(
            Cmp::try_parse(b"\x39\x48\x30", REX::new(0x49)),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Cmp,
                    op_1: Some(OpMem(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: Some(r8()),
                            index: None,
                            scale: None,
                            displacement: Some(Displacement::Byte(48_i8))
                        }
                    })),
                    op_2: Some(OpReg(rcx())),
                    op_3: None
                }
            )),
            "49 39 48 30 	cmpq	%rcx, 48(%r8)"
        )
    }
}
