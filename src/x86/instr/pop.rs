use nom::*;

use crate::x86::{
    instr::{DecodeInstruction, REX},
    Instruction, Opcode,
    Operand::Register as OpReg,
    Register, RegisterWidth,
};

#[derive(Debug, PartialEq)]
crate struct Pop {}

impl DecodeInstruction for Pop {
    // FIXME: We're just punting on register width...
    #[allow(clippy::cyclomatic_complexity)]
    fn try_parse(input: &[u8], rex: Option<REX>) -> IResult<&[u8], Instruction> {
        let rex = rex.unwrap_or_else(|| REX::new(0).unwrap());

        bits!(
            input,
            do_parse!(
                tag_bits!(u8, 5, 0x0b)
                    >> reg_bits: take_bits!(u8, 3)
                    >> reg: value!(Register::decode(
                        reg_bits + if rex.b { 0x08 } else { 0x00 },
                        RegisterWidth::QWord
                    ))
                    >> (Instruction {
                        opcode: Opcode::Pop,
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

    use crate::x86::{ctors::*, Operand::Register as OpReg};

    #[test]
    fn instr_pop_58() {
        assert_eq!(
            Pop::try_parse(b"\x58", None),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Pop,
                    op_1: Some(OpReg(rax())),
                    op_2: None,
                    op_3: None
                }
            )),
            "pop %rax"
        );

        assert_eq!(
            Pop::try_parse(b"\x59", None),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Pop,
                    op_1: Some(OpReg(rcx())),
                    op_2: None,
                    op_3: None
                }
            )),
            "pop %rcx"
        );

        assert_eq!(
            Pop::try_parse(b"\x5a", None),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Pop,
                    op_1: Some(OpReg(rdx())),
                    op_2: None,
                    op_3: None
                }
            )),
            "pop %rdx"
        );

        assert_eq!(
            Pop::try_parse(b"\x5b", None),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Pop,
                    op_1: Some(OpReg(rbx())),
                    op_2: None,
                    op_3: None
                }
            )),
            "pop %rbx"
        );

        assert_eq!(
            Pop::try_parse(b"\x5c", None),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Pop,
                    op_1: Some(OpReg(rsp())),
                    op_2: None,
                    op_3: None
                }
            )),
            "pop %rsp"
        );

        assert_eq!(
            Pop::try_parse(b"\x5d", None),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Pop,
                    op_1: Some(OpReg(rbp())),
                    op_2: None,
                    op_3: None
                }
            )),
            "pop %rbp"
        );

        assert_eq!(
            Pop::try_parse(b"\x5e", None),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Pop,
                    op_1: Some(OpReg(rsi())),
                    op_2: None,
                    op_3: None
                }
            )),
            "pop %rsi"
        );

        assert_eq!(
            Pop::try_parse(b"\x5f", None),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Pop,
                    op_1: Some(OpReg(rdi())),
                    op_2: None,
                    op_3: None
                }
            )),
            "pop %rdi"
        );
    }
}
