use nom::*;

use crate::x86::{
    instr::{
        DecodeInstruction, ImmediateBuilder, Instruction, Opcode, Operand::Register as OpReg,
        PrefixBytes,
    },
    prefix::{Group3, Prefix},
    register::Register,
    Width,
};

#[derive(Debug, PartialEq)]
pub(crate) struct Mov {}

impl DecodeInstruction for Mov {
    fn try_parse(input: &[u8], prefix: PrefixBytes) -> IResult<&[u8], Instruction> {
        if let Some(group3) = prefix.prefix.group3() {
            call!(input, Mov::parse_xc7_w, prefix)
        } else {
            alt!(
                input,
                call!(Mov::parse_x88, prefix)
                    | call!(Mov::parse_x89, prefix)
                    | call!(Mov::parse_x8a, prefix)
                    | call!(Mov::parse_x8b, prefix)
                    | call!(Mov::parse_xb8, prefix)
                    | call!(Mov::parse_xc6, prefix)
                    | call!(Mov::parse_xc7, prefix)
            )
        }
    }
}

impl Mov {
    // 88 /r            => MOV r/m8, r8
    // REX + 88 /r      => MOV r/m8, r8
    instr!(parse_x88, Opcode::Mov, Width::Byte, [0x88], r/m8, /r8);

    // 89 /r            => MOV r/m16, r16
    // 89 /r            => MOV r/m32, r32
    // REX.W + 89 /r    => MOV r/m64, r64
    instr!(parse_x89, Opcode::Mov, Width::DWord, [0x89], r/m32, /r32);

    // 8b /r            => MOV r16, r/m16
    // 8b /r            => MOV r32, r/m32
    // REX.W + 8b /r    => MOV r64, r/m64
    instr!(parse_x8b, Opcode::Mov, Width::DWord, [0x8b], /r32, r/m32);

    // b8+ rw iw            => MOV r16, imm16
    // b8+ rd id            => MOV r32, imm32
    // REX.W + b8+ rd io    => MOV r64, imm64
    #[allow(clippy::cognitive_complexity)]
    fn parse_xb8(i: &[u8], prefix: PrefixBytes) -> IResult<&[u8], (Instruction)> {
        let width = if let Some(rex) = prefix.rex() {
            if rex.w {
                Width::QWord
            } else {
                Width::DWord
            }
        } else {
            Width::DWord
        };
        do_parse!(
            i,
            reg: bits!(do_parse!(
                tag_bits!(u8, 5, 0x17)
                    >> reg_bits: take_bits!(u8, 3)
                    >> reg: value!(Register::ro(reg_bits, prefix.rex()))
                    >> (reg)
            )) >> imm: le_i32
                >> (Instruction {
                    opcode: Opcode::Mov,
                    width,
                    op_1: Some(OpReg(reg)),
                    op_2: Some(ImmediateBuilder::new(Width::DWord).signed(imm)),
                    op_3: None
                })
        )
    }

    // 8a /r            => MOV r8, r/m8
    // REX + 8a /r      => MOV r8, r/m8
    instr!(parse_x8a, Opcode::Mov, Width::DWord, [0x8a], /r8, r/m8);

    // c6 /0 ib         => MOV r/m8, imm8
    // REX c6 /0 ib     => MOV r/m8, imm8
    instr!(parse_xc6, Opcode::Mov, Width::Byte, [0xc6]+/0, r/m8, imm8);

    // c7 /0 iw             => MOV r/m16, imm16
    // c7 /0 id             => MOV r/m32, imm32
    // REX.W + c7 /0 id     => MOV r/m64, imm32
    instr!(parse_xc7_w, Opcode::Mov, Width::Word, [0xc7]+/0, r/m16, imm16);
    instr!(parse_xc7, Opcode::Mov, Width::DWord, [0xc7]+/0, r/m32, imm32);
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
    fn instr_mov_88() {
        assert_eq!(
            Mov::try_parse(b"\x88\x10", PrefixBytes::new_none()),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Mov,
                    width: Width::Byte,
                    op_1: Some(OpMem(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: Some(rax()),
                            index: None,
                            scale: None,
                            displacement: None
                        }
                    })),
                    op_2: Some(OpReg(dl())),
                    op_3: None
                }
            )),
            "88 10   movb    %dl, (%rax)"
        );
    }

    #[test]
    fn instr_mov_89() {
        assert_eq!(
            Mov::try_parse(b"\x89\xe2", PrefixBytes::new_rex(0x48)),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Mov,
                    width: Width::QWord,
                    op_1: Some(OpReg(rdx())),
                    op_2: Some(OpReg(rsp())),
                    op_3: None
                }
            )),
            "48 89 e2                movq    rdx, rsp"
        );

        assert_eq!(
            Mov::try_parse(b"\x89\xf6", PrefixBytes::new_rex(0x49)),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Mov,
                    width: Width::QWord,
                    op_1: Some(OpReg(r14())),
                    op_2: Some(OpReg(rsi())),
                    op_3: None
                }
            )),
            "49 89 f6                movq    r14, rsi"
        );
    }

    #[test]
    fn instr_mov_8b() {
        assert_eq!(
            Mov::try_parse(b"\x8b\x46\x60", PrefixBytes::new_rex(0x4c)),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Mov,
                    width: Width::QWord,
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
            Mov::try_parse(b"\xb8\x01\x00\x00\x00", PrefixBytes::new_none()),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Mov,
                    width: Width::DWord,
                    op_1: Some(OpReg(rax())),
                    op_2: Some(OpImm(Immediate::DWord(1))),
                    op_3: None
                }
            )),
            "b8 01 00 00 00 	movl	$1, %eax"
        );

        assert_eq!(
            Mov::try_parse(b"\xba\xf8\xff\xff\x8c", PrefixBytes::new_none()),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Mov,
                    width: Width::DWord,
                    op_1: Some(OpReg(rdx())),
                    op_2: Some(OpImm(Immediate::DWord(-1929379848))),
                    op_3: None
                }
            )),
            "ba f8 ff ff 8c  movl    $2365587448, %edx"
        );
    }

    #[test]
    fn instr_mov_c6() {
        assert_eq!(
            Mov::try_parse(b"\xc6\x40\x02\x00", PrefixBytes::new_none()),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Mov,
                    width: Width::Byte,
                    op_1: Some(OpMem(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: Some(rax()),
                            index: None,
                            scale: None,
                            displacement: Some(Displacement::Byte(2))
                        }
                    })),
                    op_2: Some(OpImm(Immediate::Byte(0))),
                    op_3: None,
                }
            )),
            "c6 40 02 00     movb    $0, 2(%rax)"
        );
    }

    #[test]
    fn instr_mov_c7() {
        assert_eq!(
            Mov::try_parse(
                b"\xc7\x05\x00\x52\x00\x00\x50\x00\x00\x00",
                PrefixBytes::new_none()
            ),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Mov,
                    width: Width::DWord,
                    op_1: Some(OpMem(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: Some(rip()),
                            index: None,
                            scale: None,
                            displacement: Some(Displacement::DWord(20992))
                        }
                    })),
                    op_2: Some(OpImm(Immediate::DWord(80))),
                    op_3: None,
                }
            )),
            "c7 05 00 52 00 00 50 00 00 00   movl    $80, 20992(%rip)"
        );

        assert_eq!(
            Mov::try_parse(b"\xc7\x00\x30\x3a", PrefixBytes::new_prefix(b"\x66\xc7")),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Mov,
                    width: Width::Word,
                    op_1: Some(OpMem(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: Some(rax()),
                            index: None,
                            scale: None,
                            displacement: None
                        }
                    })),
                    op_2: Some(OpImm(Immediate::Word(14896))),
                    op_3: None,
                }
            )),
            "66 c7 00 30 3a  movw    $14896, (%rax)"
        );
    }
}
