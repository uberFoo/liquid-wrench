use nom::*;

use crate::x86::{
    instr::{DecodeInstruction, Instruction, Opcode, PrefixBytes},
    Width,
};

#[derive(Debug, PartialEq)]
pub(crate) struct And {}

impl DecodeInstruction for And {
    fn try_parse(input: &[u8], prefix: PrefixBytes, address: usize) -> IResult<&[u8], Instruction> {
        alt!(
            input,
            call!(And::parse_x20, prefix, address)
                | call!(And::parse_x21, prefix, address)
                | call!(And::parse_x24, prefix, address)
                | call!(And::parse_x25, prefix, address)
                | call!(And::parse_x80, prefix, address)
                | call!(And::parse_x81, prefix, address)
                | call!(And::parse_x83, prefix, address)
        )
    }
}

impl And {
    // 20 /r	        => AND r/m8, r8
    // REX + 20 /r      => AND r/m8, r8
    #[rustfmt::skip]
    instr!(parse_x20, Opcode::And, Width::Byte, [0x20], r/m8, /r8);

    // 21 /r            => AND r/m16, r16
    // 21 /r            => AND r/m32, r32
    // REX.W + 21 /r    => AND r/m64, r64
    #[rustfmt::skip]
    instr!(parse_x21, Opcode::And, Width::DWord, [0x21], r/m32, /r32);

    // 24 /r            => AND AL, imm8
    #[rustfmt::skip]
    instr!(parse_x24, Opcode::And, Width::Byte, [0x24], reg:al, imm8);

    // 25 iw            => AND AX, imm16
    // 25 id            => AND EAX, imm32
    // REX.W + 25 id    => AND RAX, imm32
    #[rustfmt::skip]
    instr!(parse_x25, Opcode::And, Width::DWord, [0x25], reg:eax, imm32);

    // 80 /4 ib	        => AND r/m8, imm8
    // REX + 80 /4 ib	=> AND r/m8, imm8
    instr!(parse_x80, Opcode::And, Width::Byte, [0x80]+/4, r/m8, imm8);

    // 81 /4 iw         => AND r/m16, imm16
    // 81 /4 id         => AND r/m32, imm32
    // REX.W + 81 /4 id => AND r/m64, imm32
    instr!(parse_x81, Opcode::And, Width::DWord, [0x81]+/4, r/m32, imm32);

    // 83 /4 ib         => AND r/m16, imm8
    // 83 /4 ib         => AND r/m32, imm8
    // REX.W + 83 /4 ib => AND r/m64, imm8
    instr!(parse_x83, Opcode::And, Width::DWord, [0x83]+/4, r/m32, imm8);
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::x86::{
        instr::{
            Immediate,
            Operand::{Immediate as OpImm, Register as OpReg},
        },
        register::ctors::*,
    };

    #[test]
    fn instr_and_20() {
        assert_eq!(
            And::try_parse(b"\x20\xcb", PrefixBytes::new_none(), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::And,
                    width: Width::Byte,
                    op_1: Some(OpReg(bl())),
                    op_2: Some(OpReg(cl())),
                    op_3: None
                }
            )),
            "20 cb 	andb	%cl, %bl"
        );

        assert_eq!(
            And::try_parse(b"\x20\xf1", PrefixBytes::new_rex(0x40), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::And,
                    width: Width::Byte,
                    op_1: Some(OpReg(cl())),
                    op_2: Some(OpReg(sil())),
                    op_3: None
                }
            )),
            "40 20 f1 	andb	%sil, %cl"
        );

        assert_eq!(
            And::try_parse(b"\x20\xf0", PrefixBytes::new_rex(0x44), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::And,
                    width: Width::Byte,
                    op_1: Some(OpReg(al())),
                    op_2: Some(OpReg(r14b())),
                    op_3: None
                }
            )),
            "44 20 f0 	andb	%r14b, %al"
        );
    }

    #[test]
    fn instr_and_21() {
        assert_eq!(
            And::try_parse(b"\x21\xc8", PrefixBytes::new_none(), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::And,
                    width: Width::DWord,
                    op_1: Some(OpReg(eax())),
                    op_2: Some(OpReg(ecx())),
                    op_3: None
                }
            )),
            "21 c8 	andl	%ecx, %eax"
        );

        assert_eq!(
            And::try_parse(b"\x21\xc1", PrefixBytes::new_rex(0x48), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::And,
                    width: Width::QWord,
                    op_1: Some(OpReg(rcx())),
                    op_2: Some(OpReg(rax())),
                    op_3: None
                }
            )),
            "48 21 c1 	andq	%rax, %rcx"
        );
    }

    #[test]
    fn instr_and_24() {
        assert_eq!(
            And::try_parse(b"\x24\xfe", PrefixBytes::new_none(), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::And,
                    width: Width::Byte,
                    op_1: Some(OpReg(al())),
                    op_2: Some(OpImm(Immediate::Byte(-2_i8))),
                    op_3: None
                }
            )),
            "24 fe      andb    $-2, %al"
        );
    }

    /// FIXME: Exercise this instruction with a REX.W bit.
    #[test]
    fn instr_and_25() {
        assert_eq!(
            And::try_parse(b"\x25\x00\xf0\x00\x00", PrefixBytes::new_none(), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::And,
                    width: Width::DWord,
                    op_1: Some(OpReg(eax())),
                    op_2: Some(OpImm(Immediate::DWord(61_440_i32))),
                    op_3: None
                }
            )),
            "25 00 f0 00 00 	andl	$61440, %eax"
        )
    }

    #[test]
    fn instr_and_80() {
        assert_eq!(
            And::try_parse(b"\x80\xe1\xf8", PrefixBytes::new_none(), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::And,
                    width: Width::Byte,
                    op_1: Some(OpReg(cl())),
                    op_2: Some(OpImm(Immediate::Byte(-8_i8))),
                    op_3: None
                }
            )),
            "80 e1 f8 	andb	$-8, %cl"
        )
    }

    #[test]
    fn instr_and_81() {
        // Note that 4294966254 is -1042.  The disassembly comes from objdump,
        // and I don't trust it.  Neither do I entirely trust my conversion to
        // an i32, but that conversion is what the reference indicates.
        assert_eq!(
            And::try_parse(b"\x81\xe1\xee\xfb\xff\xff", PrefixBytes::new_none(), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::And,
                    width: Width::DWord,
                    op_1: Some(OpReg(ecx())),
                    // FIXME: Why is this unsigned?
                    op_2: Some(OpImm(Immediate::DWord(-1042_i32))),
                    op_3: None
                }
            )),
            "81 e1 ee fb ff ff 	andl	$4294966254, %ecx"
        );

        assert_eq!(
            And::try_parse(b"\x81\xe6\x00\xf0\x00\x00", PrefixBytes::new_rex(0x41), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::And,
                    width: Width::DWord,
                    op_1: Some(OpReg(r14d())),
                    op_2: Some(OpImm(Immediate::DWord(61_440_i32))),
                    op_3: None
                }
            )),
            "41 81 e6 00 f0 00 00 	andl	$61440, %r14d"
        )
    }

    #[test]
    fn instr_and_83() {
        assert_eq!(
            And::try_parse(b"\x83\xe1\x0f", PrefixBytes::new_none(), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::And,
                    width: Width::DWord,
                    op_1: Some(OpReg(ecx())),
                    op_2: Some(OpImm(Immediate::Byte(15_i8))),
                    op_3: None
                }
            )),
            "83 e1 0f 	andl	$15, %ecx"
        );

        assert_eq!(
            And::try_parse(b"\x83\xe7\x07", PrefixBytes::new_none(), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::And,
                    width: Width::DWord,
                    op_1: Some(OpReg(edi())),
                    op_2: Some(OpImm(Immediate::Byte(7_i8))),
                    op_3: None
                }
            )),
            "83 e7 07 	andl	$7, %edi"
        );

        assert_eq!(
            And::try_parse(b"\x83\xe4\xed", PrefixBytes::new_rex(0x41), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::And,
                    width: Width::DWord,
                    op_1: Some(OpReg(r12d())),
                    op_2: Some(OpImm(Immediate::Byte(-19_i8))),
                    op_3: None
                }
            )),
            "41 83 e4 ed 	andl	$-19, %r12d"
        );

        assert_eq!(
            And::try_parse(b"\x83\xe4\xf0", PrefixBytes::new_rex(0x48), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::And,
                    width: Width::QWord,
                    op_1: Some(OpReg(rsp())),
                    op_2: Some(OpImm(Immediate::Byte(-16_i8))),
                    op_3: None
                }
            )),
            "48 83 e4 f0 	andq   $-16, %rsp"
        );
    }
}
