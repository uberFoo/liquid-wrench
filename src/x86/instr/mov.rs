use nom::*;

use crate::x86::{
    instr::{
        DecodeInstruction, ImmediateBuilder, Instruction, Opcode, Operand::Register as OpReg, REX,
    },
    register::Register,
    Width,
};

#[derive(Debug, PartialEq)]
crate struct Mov {}

impl DecodeInstruction for Mov {
    fn try_parse(input: &[u8], rex: Option<REX>) -> IResult<&[u8], Instruction> {
        alt!(
            input,
            call!(Mov::parse_x89, rex) | call!(Mov::parse_x8b, rex) | call!(Mov::parse_xb8, rex)
        )
    }
}

impl Mov {
    // 89 /r            => MOV r/m16, r16
    // 89 /r            => MOV r/m32, r32
    // REX.W + 89 /r    => MOV r/m64, r64
    instr!(parse_x89, Opcode::Mov, [0x89], r/m32, /r32);

    // 8B /r            => MOV r16, r/m16
    // 8B /r            => MOV r32, r/m32
    // REX.W + 8B /r    => MOV r64, r/m64
    instr!(parse_x8b, Opcode::Mov, [0x8b], /r32, r/m32);

    // B8+ rw iw            => MOV r16, imm16
    // B8+ rd id            => MOV r32, imm32
    // REX.W + B8+ rd io    => MOV r64, imm64
    named_args!(
        parse_xb8(rex: Option<REX>)<Instruction>,
        do_parse!(
            reg: bits!(
                do_parse!(
                    tag_bits!(u8, 5, 0x17)
                    >> reg_bits: take_bits!(u8, 3)
                    >> reg: value!(Register::ro(reg_bits, rex))
                    >> (reg)
                )
            )
            >> imm: le_i32
            >> (Instruction {
                opcode: Opcode::Mov,
                op_1: Some(OpReg(reg)),
                    op_2: Some(ImmediateBuilder::new(Width::DWord).signed(imm)),
                op_3: None
            })
        )
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::x86::{
        instr::{
            Displacement, EffectiveAddress, Immediate, LogicalAddress,
            Operand::{Immediate as OpImm, Memory as OpMem, Register as OpReg},
        },
        register::ctors::*,
    };

    #[test]
    fn instr_mov_89() {
        assert_eq!(
            Mov::try_parse(b"\x89\xe2", REX::new(0x48)),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Mov,
                    op_1: Some(OpReg(rdx())),
                    op_2: Some(OpReg(rsp())),
                    op_3: None
                }
            )),
            "48 89 e2                mov    rdx, rsp"
        );

        assert_eq!(
            Mov::try_parse(b"\x89\xf6", REX::new(0x49)),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Mov,
                    op_1: Some(OpReg(r14())),
                    op_2: Some(OpReg(rsi())),
                    op_3: None
                }
            )),
            "49 89 f6                mov    r14, rsi"
        );
    }

    #[test]
    fn instr_mov_8b() {
        assert_eq!(
            Mov::try_parse(b"\x8b\x46\x60", REX::new(0x4c)),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Mov,
                    op_1: Some(OpReg(r8())),
                    op_2: Some(OpMem(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: Some(rsi()),
                            index: None,
                            scale: None,
                            displacement: Some(Displacement::Byte(96_i8))
                        }
                    })),
                    op_3: None,
                }
            )),
            "4c 8b 46 60 	movq	96(%rsi), %r8"
        );
    }

    #[test]
    fn instr_mov_b8() {
        assert_eq!(
            Mov::try_parse(b"\xb8\x01\x00\x00\x00", None),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Mov,
                    op_1: Some(OpReg(rax())),
                    op_2: Some(OpImm(Immediate::DWord(1))),
                    op_3: None
                }
            )),
            "b8 01 00 00 00 	movl	$1, %eax"
        );
    }
}
