use nom::*;

use crate::x86::{
    instr::{DecodeInstruction, Instruction, Opcode, Operand::Register as OpReg, PrefixBytes},
    register::Register,
    Width,
};

#[derive(Debug, PartialEq)]
pub(crate) struct Push {}

impl DecodeInstruction for Push {
    fn try_parse(input: &[u8], prefix: PrefixBytes) -> IResult<&[u8], Instruction> {
        alt!(input, call!(Push::parse_x50, prefix))
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
        parse_x50(prefix: PrefixBytes)<Instruction>,
        bits!(
            do_parse!(
                tag_bits!(u8, 5, 0x0a)
                >> reg_bits: take_bits!(u8, 3)
                >> reg: value!(Register::ro(reg_bits, prefix.rex()))
                >> (Instruction {
                    opcode: Opcode::Push,
                    width: Width::QWord,
                    op_1: Some(OpReg(reg)),
                    op_2: None,
                    op_3: None
                })
            )
        )
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::x86::{register::ctors::*, Width};

    #[test]
    fn instr_push_50() {
        assert_eq!(
            Push::try_parse(b"\x50", PrefixBytes::new_none()),
            Ok((
                &b""[..],
                Instruction {
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
            Push::try_parse(b"\x51", PrefixBytes::new_none()),
            Ok((
                &b""[..],
                Instruction {
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
            Push::try_parse(b"\x52", PrefixBytes::new_none()),
            Ok((
                &b""[..],
                Instruction {
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
            Push::try_parse(b"\x53", PrefixBytes::new_none()),
            Ok((
                &b""[..],
                Instruction {
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
            Push::try_parse(b"\x54", PrefixBytes::new_none()),
            Ok((
                &b""[..],
                Instruction {
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
            Push::try_parse(b"\x55", PrefixBytes::new_none()),
            Ok((
                &b""[..],
                Instruction {
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
            Push::try_parse(b"\x56", PrefixBytes::new_none()),
            Ok((
                &b""[..],
                Instruction {
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
            Push::try_parse(b"\x57", PrefixBytes::new_none()),
            Ok((
                &b""[..],
                Instruction {
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
            Push::try_parse(b"\x50", PrefixBytes::new_rex(0x41)),
            Ok((
                &b""[..],
                Instruction {
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
            Push::try_parse(b"\x51", PrefixBytes::new_rex(0x41)),
            Ok((
                &b""[..],
                Instruction {
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
            Push::try_parse(b"\x52", PrefixBytes::new_rex(0x41)),
            Ok((
                &b""[..],
                Instruction {
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
            Push::try_parse(b"\x53", PrefixBytes::new_rex(0x41)),
            Ok((
                &b""[..],
                Instruction {
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
            Push::try_parse(b"\x54", PrefixBytes::new_rex(0x41)),
            Ok((
                &b""[..],
                Instruction {
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
            Push::try_parse(b"\x55", PrefixBytes::new_rex(0x41)),
            Ok((
                &b""[..],
                Instruction {
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
            Push::try_parse(b"\x56", PrefixBytes::new_rex(0x41)),
            Ok((
                &b""[..],
                Instruction {
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
            Push::try_parse(b"\x57", PrefixBytes::new_rex(0x41)),
            Ok((
                &b""[..],
                Instruction {
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
}
