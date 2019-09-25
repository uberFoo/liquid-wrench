use nom::*;

use crate::x86::{
    instr::{DecodeInstruction, Instruction, Opcode, REX},
    Width,
};

#[derive(Debug, PartialEq)]
crate struct Cmp {}

impl DecodeInstruction for Cmp {
    fn try_parse(input: &[u8], rex: Option<REX>) -> IResult<&[u8], Instruction> {
        alt!(
            input,
            call!(Cmp::parse_x38, rex)
                | call!(Cmp::parse_x39, rex)
                | call!(Cmp::parse_x80, rex)
                | call!(Cmp::parse_x83, rex)
        )
    }
}

impl Cmp {
    // 38 /r            => CMP r/m8, r8
    // REX.W c 38 /r    => CMP r/m8, r8
    instr!(parse_x38, Opcode::Cmp, [0x38], r/m8, /r8);

    // 39 /r            => CMP r/m16, r16
    // 39 /r            => CMP r/m32, r32
    // REX.W + 39 /r    => CMP r/m64, r64
    instr!(parse_x39, Opcode::Cmp, [0x39], r/m32, /r32);

    // 80 /7 ib             => CMP r/m8, imm8
    // REX.W + 80 /7 ib     => CMP r/m8, imm8
    instr!(parse_x80, Opcode::Cmp, [0x80]+/7, r/m8, imm8);

    // 83 /7 ib             => CMP r/m16, imm8
    // 83 /7 ib             => CMP r/m32, imm8
    // REX.W + 83 /7 ib     => CMP r/m64, imm8
    instr!(parse_x83, Opcode::Cmp, [0x83]+/7, r/m32, imm8);
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
