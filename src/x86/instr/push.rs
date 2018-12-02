use nom::*;

use crate::x86::{
    instr::{DecodeInstruction, Instruction, Opcode, Operand::Register as OpReg, REX},
    register::Register,
    Width,
};

#[derive(Debug, PartialEq)]
crate struct Push {}

impl DecodeInstruction for Push {
    // FIXME: We're just punting on register width...
    #[allow(clippy::cyclomatic_complexity)]
    fn try_parse(input: &[u8], rex: Option<REX>) -> IResult<&[u8], Instruction> {
        let rex = rex.unwrap_or_else(|| REX::new(0).unwrap());

        bits!(
            input,
            do_parse!(
                tag_bits!(u8, 5, 0x0a)
                    >> reg_bits: take_bits!(u8, 3)
                    >> reg: value!(Register::decode(
                        reg_bits + if rex.b { 0x08 } else { 0x00 },
                        Width::QWord
                    ))
                    >> (Instruction {
                        opcode: Opcode::Push,
                        op_1: Some(OpReg(reg)),
                        op_2: None,
                        op_3: None
                    })
            )
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::x86::register::ctors::*;

    #[test]
    fn instr_push_50() {
        assert_eq!(
            Push::try_parse(b"\x50", None),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Push,
                    op_1: Some(OpReg(rax())),
                    op_2: None,
                    op_3: None
                }
            )),
            "push %rax"
        );

        assert_eq!(
            Push::try_parse(b"\x51", None),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Push,
                    op_1: Some(OpReg(rcx())),
                    op_2: None,
                    op_3: None
                }
            )),
            "push %rcx"
        );

        assert_eq!(
            Push::try_parse(b"\x52", None),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Push,
                    op_1: Some(OpReg(rdx())),
                    op_2: None,
                    op_3: None
                }
            )),
            "push %rdx"
        );

        assert_eq!(
            Push::try_parse(b"\x53", None),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Push,
                    op_1: Some(OpReg(rbx())),
                    op_2: None,
                    op_3: None
                }
            )),
            "push %rbx"
        );

        assert_eq!(
            Push::try_parse(b"\x54", None),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Push,
                    op_1: Some(OpReg(rsp())),
                    op_2: None,
                    op_3: None
                }
            )),
            "push %rsp"
        );

        assert_eq!(
            Push::try_parse(b"\x55", None),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Push,
                    op_1: Some(OpReg(rbp())),
                    op_2: None,
                    op_3: None
                }
            )),
            "push %rpb"
        );

        assert_eq!(
            Push::try_parse(b"\x56", None),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Push,
                    op_1: Some(OpReg(rsi())),
                    op_2: None,
                    op_3: None
                }
            )),
            "push %rsi"
        );

        assert_eq!(
            Push::try_parse(b"\x57", None),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Push,
                    op_1: Some(OpReg(rdi())),
                    op_2: None,
                    op_3: None
                }
            )),
            "push %rdi"
        );
    }
}
