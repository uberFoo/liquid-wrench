use nom::*;

use crate::x86::{
    instr::{DecodeInstruction, Instruction, Opcode, Operand::Register as OpReg, PrefixBytes},
    register::Register,
    Width,
};

#[derive(Debug, PartialEq)]
pub(crate) struct Pop {}

impl DecodeInstruction for Pop {
    fn try_parse(input: &[u8], prefix: PrefixBytes, address: usize) -> IResult<&[u8], Instruction> {
        alt!(input, call!(Pop::parse_x58, prefix, address))
    }
}

impl Pop {
    /// Opcode 58+{rw, rd, rd} | POP {r16, R32, R64}
    ///
    /// The first 5 bits [7-3] indicate that this is a POP:
    ///
    /// |0|1|0|1|1|x|x|x|
    ///
    /// Read as a u8, their value is 0x0b.  The last three bits, optionally with the REX byte,
    /// encode the specific register being popped.
    ///
    /// Since we're currently only concerning ourselves with 64-bit mode, this will always treat
    /// the register operand as being 64-bits wide.  This seems to imply that there should be an
    /// Opcode `58+ ro`, but it's not in the reference.
    named_args!(
        parse_x58(prefix: PrefixBytes, address: usize)<Instruction>,
        bits!(
            do_parse!(
                tag_bits!(u8, 5, 0x0b)
                >> reg_bits: take_bits!(u8, 3)
                >> reg: value!(Register::ro(reg_bits, prefix.rex()))
                >> (Instruction {
                    address,
                    width: Width::QWord,
                    opcode: Opcode::Pop,
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

    use crate::x86::{instr::Operand::Register as OpReg, register::ctors::*};

    #[test]
    fn instr_pop_58() {
        assert_eq!(
            Pop::try_parse(b"\x58", PrefixBytes::new_none(), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Pop,
                    width: Width::QWord,
                    op_1: Some(OpReg(rax())),
                    op_2: None,
                    op_3: None
                }
            )),
            "popq %rax"
        );

        assert_eq!(
            Pop::try_parse(b"\x59", PrefixBytes::new_none(), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Pop,
                    width: Width::QWord,
                    op_1: Some(OpReg(rcx())),
                    op_2: None,
                    op_3: None
                }
            )),
            "popq %rcx"
        );

        assert_eq!(
            Pop::try_parse(b"\x5a", PrefixBytes::new_none(), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Pop,
                    width: Width::QWord,
                    op_1: Some(OpReg(rdx())),
                    op_2: None,
                    op_3: None
                }
            )),
            "popq %rdx"
        );

        assert_eq!(
            Pop::try_parse(b"\x5b", PrefixBytes::new_none(), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Pop,
                    width: Width::QWord,
                    op_1: Some(OpReg(rbx())),
                    op_2: None,
                    op_3: None
                }
            )),
            "popq %rbx"
        );

        assert_eq!(
            Pop::try_parse(b"\x5c", PrefixBytes::new_none(), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Pop,
                    width: Width::QWord,
                    op_1: Some(OpReg(rsp())),
                    op_2: None,
                    op_3: None
                }
            )),
            "popq %rsp"
        );

        assert_eq!(
            Pop::try_parse(b"\x5d", PrefixBytes::new_none(), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Pop,
                    width: Width::QWord,
                    op_1: Some(OpReg(rbp())),
                    op_2: None,
                    op_3: None
                }
            )),
            "popq %rbp"
        );

        assert_eq!(
            Pop::try_parse(b"\x5e", PrefixBytes::new_none(), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Pop,
                    width: Width::QWord,
                    op_1: Some(OpReg(rsi())),
                    op_2: None,
                    op_3: None
                }
            )),
            "popq %rsi"
        );

        assert_eq!(
            Pop::try_parse(b"\x5f", PrefixBytes::new_none(), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Pop,
                    width: Width::QWord,
                    op_1: Some(OpReg(rdi())),
                    op_2: None,
                    op_3: None
                }
            )),
            "popq %rdi"
        );

        assert_eq!(
            Pop::try_parse(b"\x58", PrefixBytes::new_rex(0x41), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Pop,
                    width: Width::QWord,
                    op_1: Some(OpReg(r8())),
                    op_2: None,
                    op_3: None
                }
            )),
            "popq %r8"
        );

        assert_eq!(
            Pop::try_parse(b"\x59", PrefixBytes::new_rex(0x41), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Pop,
                    width: Width::QWord,
                    op_1: Some(OpReg(r9())),
                    op_2: None,
                    op_3: None
                }
            )),
            "popq %r9"
        );

        assert_eq!(
            Pop::try_parse(b"\x5a", PrefixBytes::new_rex(0x41), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Pop,
                    width: Width::QWord,
                    op_1: Some(OpReg(r10())),
                    op_2: None,
                    op_3: None
                }
            )),
            "popq %r10"
        );

        assert_eq!(
            Pop::try_parse(b"\x5b", PrefixBytes::new_rex(0x41), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Pop,
                    width: Width::QWord,
                    op_1: Some(OpReg(r11())),
                    op_2: None,
                    op_3: None
                }
            )),
            "popq %r11"
        );

        assert_eq!(
            Pop::try_parse(b"\x5c", PrefixBytes::new_rex(0x41), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Pop,
                    width: Width::QWord,
                    op_1: Some(OpReg(r12())),
                    op_2: None,
                    op_3: None
                }
            )),
            "popq %r12"
        );

        assert_eq!(
            Pop::try_parse(b"\x5d", PrefixBytes::new_rex(0x41), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Pop,
                    width: Width::QWord,
                    op_1: Some(OpReg(r13())),
                    op_2: None,
                    op_3: None
                }
            )),
            "popq %r13"
        );

        assert_eq!(
            Pop::try_parse(b"\x5e", PrefixBytes::new_rex(0x41), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Pop,
                    width: Width::QWord,
                    op_1: Some(OpReg(r14())),
                    op_2: None,
                    op_3: None
                }
            )),
            "popq %r14"
        );

        assert_eq!(
            Pop::try_parse(b"\x5f", PrefixBytes::new_rex(0x41), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Pop,
                    width: Width::QWord,
                    op_1: Some(OpReg(r15())),
                    op_2: None,
                    op_3: None
                }
            )),
            "popq %r15"
        );
    }
}
