use nom::*;

use crate::x86::{
    instr::{DecodeInstruction, Instruction, Opcode, Operand::Register as OpReg, PrefixBytes},
    register::Register,
    Width,
};

#[derive(Debug, PartialEq)]
pub(crate) struct Push {}

impl DecodeInstruction for Push {
    fn try_parse(input: &[u8], prefix: PrefixBytes, address: usize) -> IResult<&[u8], Instruction> {
        alt!(
            input,
            call!(Push::parse_x50, prefix, address)
                | call!(Push::parse_x6a, prefix, address)
                | call!(Push::parse_x68, prefix, address)
                | call!(Push::parse_xff, prefix, address)
        )
    }
}

impl Push {
    /// Opcode 50+{rw, rd, rd} | PUSH {r16, R32, R64}
    ///
    /// The first 5 bits [7-3] indicate that this is a POP:
    ///
    /// |0|1|0|1|0|x|x|x|
    ///
    /// Read as a u8, their value is 0x0a.  The last three bits, optionally with the REX byte,
    /// encode the specific register being popped.
    ///
    /// Since we're currently only concerning ourselves with 64-bit mode, this will always treat
    /// the register operand as being 64-bits wide.  This seems to imply that there should be an
    /// Opcode `50+ ro`, but it's not in the reference.
    named_args!(
        parse_x50(prefix: PrefixBytes, address: usize)<Instruction>,
        bits!(
            do_parse!(
                tag_bits!(u8, 5, 0x0a)
                >> reg_bits: take_bits!(u8, 3)
                >> reg: value!(Register::ro(reg_bits, prefix.rex()))
                >> (Instruction {
                    address,
                    opcode: Opcode::Push,
                    width: Width::QWord,
                    op_1: Some(OpReg(reg)),
                    op_2: None,
                    op_3: None
                })
            )
        )
    );

    // 6a ib        => PUSH imm8
    instr!(parse_x6a, Opcode::Push, Width::QWord, [0x6a], imm8);

    // 68 iw        => PUSH imm16
    // 68 id        => PUSH imm32
    instr!(parse_x68, Opcode::Push, Width::QWord, [0x68], imm32);

    // ff /6        => PUSH r/m16
    // ff /6        => PUSH r/m32
    // ff /6        => PUSH r/m64
    instr!(parse_xff, Opcode::Push, Width::QWord, [0xff]+/6, r/m64);
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::x86::{
        instr::{
            Displacement, EffectiveAddress, Immediate, LogicalAddress,
            Operand::{Immediate as OpImm, Memory as OpMem},
        },
        register::ctors::*,
    };

    #[test]
    fn instr_push_50() {
        assert_eq!(
            Push::try_parse(b"\x50", PrefixBytes::new_none(), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Push,
                    width: Width::QWord,
                    op_1: Some(OpReg(rax())),
                    op_2: None,
                    op_3: None
                }
            )),
            "pushq %rax"
        );

        assert_eq!(
            Push::try_parse(b"\x51", PrefixBytes::new_none(), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Push,
                    width: Width::QWord,
                    op_1: Some(OpReg(rcx())),
                    op_2: None,
                    op_3: None
                }
            )),
            "pushq %rcx"
        );

        assert_eq!(
            Push::try_parse(b"\x52", PrefixBytes::new_none(), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Push,
                    width: Width::QWord,
                    op_1: Some(OpReg(rdx())),
                    op_2: None,
                    op_3: None
                }
            )),
            "pushq %rdx"
        );

        assert_eq!(
            Push::try_parse(b"\x53", PrefixBytes::new_none(), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Push,
                    width: Width::QWord,
                    op_1: Some(OpReg(rbx())),
                    op_2: None,
                    op_3: None
                }
            )),
            "pushq %rbx"
        );

        assert_eq!(
            Push::try_parse(b"\x54", PrefixBytes::new_none(), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Push,
                    width: Width::QWord,
                    op_1: Some(OpReg(rsp())),
                    op_2: None,
                    op_3: None
                }
            )),
            "pushq %rsp"
        );

        assert_eq!(
            Push::try_parse(b"\x55", PrefixBytes::new_none(), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Push,
                    width: Width::QWord,
                    op_1: Some(OpReg(rbp())),
                    op_2: None,
                    op_3: None
                }
            )),
            "pushq %rpb"
        );

        assert_eq!(
            Push::try_parse(b"\x56", PrefixBytes::new_none(), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Push,
                    width: Width::QWord,
                    op_1: Some(OpReg(rsi())),
                    op_2: None,
                    op_3: None
                }
            )),
            "pushq %rsi"
        );

        assert_eq!(
            Push::try_parse(b"\x57", PrefixBytes::new_none(), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Push,
                    width: Width::QWord,
                    op_1: Some(OpReg(rdi())),
                    op_2: None,
                    op_3: None
                }
            )),
            "pushq %rdi"
        );

        assert_eq!(
            Push::try_parse(b"\x50", PrefixBytes::new_rex(0x41), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Push,
                    width: Width::QWord,
                    op_1: Some(OpReg(r8())),
                    op_2: None,
                    op_3: None
                }
            )),
            "pushq %r8"
        );

        assert_eq!(
            Push::try_parse(b"\x51", PrefixBytes::new_rex(0x41), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Push,
                    width: Width::QWord,
                    op_1: Some(OpReg(r9())),
                    op_2: None,
                    op_3: None
                }
            )),
            "pushq %r9"
        );

        assert_eq!(
            Push::try_parse(b"\x52", PrefixBytes::new_rex(0x41), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Push,
                    width: Width::QWord,
                    op_1: Some(OpReg(r10())),
                    op_2: None,
                    op_3: None
                }
            )),
            "pushq %r10"
        );

        assert_eq!(
            Push::try_parse(b"\x53", PrefixBytes::new_rex(0x41), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Push,
                    width: Width::QWord,
                    op_1: Some(OpReg(r11())),
                    op_2: None,
                    op_3: None
                }
            )),
            "pushq %r11"
        );

        assert_eq!(
            Push::try_parse(b"\x54", PrefixBytes::new_rex(0x41), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Push,
                    width: Width::QWord,
                    op_1: Some(OpReg(r12())),
                    op_2: None,
                    op_3: None
                }
            )),
            "pushq %r12"
        );

        assert_eq!(
            Push::try_parse(b"\x55", PrefixBytes::new_rex(0x41), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Push,
                    width: Width::QWord,
                    op_1: Some(OpReg(r13())),
                    op_2: None,
                    op_3: None
                }
            )),
            "pushq %r13"
        );

        assert_eq!(
            Push::try_parse(b"\x56", PrefixBytes::new_rex(0x41), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Push,
                    width: Width::QWord,
                    op_1: Some(OpReg(r14())),
                    op_2: None,
                    op_3: None
                }
            )),
            "pushq %r14"
        );

        assert_eq!(
            Push::try_parse(b"\x57", PrefixBytes::new_rex(0x41), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Push,
                    width: Width::QWord,
                    op_1: Some(OpReg(r15())),
                    op_2: None,
                    op_3: None
                }
            )),
            "pushq %r15"
        );
    }

    #[test]
    fn instr_push_ff() {
        assert_eq!(
            Push::try_parse(b"\xff\xb0\xff\xff\xff\x75", PrefixBytes::new_none(), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Push,
                    width: Width::QWord,
                    op_1: Some(OpMem(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: Some(rax()),
                            index: None,
                            scale: None,
                            displacement: Some(Displacement::DWord(1979711487))
                        }
                    })),
                    op_2: None,
                    op_3: None
                }
            )),
            "ff b0 ff ff ff 75       pushq   1979711487(%rax)"
        )
    }

    #[test]
    fn instr_push_6a() {
        assert_eq!(
            Push::try_parse(b"\x6a\x01", PrefixBytes::new_none(), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Push,
                    width: Width::QWord,
                    op_1: Some(OpImm(Immediate::Byte(1))),
                    op_2: None,
                    op_3: None
                }
            )),
            "6a 01   pushq   $1"
        )
    }

    #[test]
    fn instr_push_68() {
        assert_eq!(
            Push::try_parse(b"\x68\x79\x00\x00\x00", PrefixBytes::new_none(), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Push,
                    width: Width::QWord,
                    op_1: Some(OpImm(Immediate::DWord(121))),
                    op_2: None,
                    op_3: None
                }
            )),
            "68 79 00 00 00  pushq   $121"
        )
    }
}
