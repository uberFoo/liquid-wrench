use nom::*;

use crate::x86::instr::{DecodeInstruction, Instruction, Opcode, REX};

#[derive(Debug, PartialEq)]
pub(crate) struct Movzx {}

impl DecodeInstruction for Movzx {
    fn try_parse(input: &[u8], rex: Option<REX>) -> IResult<&[u8], Instruction> {
        alt!(
            input,
            call!(Movzx::parse_x0fb6, rex) | call!(Movzx::parse_x0fb7, rex)
        )
    }
}

impl Movzx {
    // 0f b6 /r            => MOV r16, r/m8
    // 0f 86 /r            => MOV r32, r/m8
    // REX.W + 0f b6 /r    => MOV r64, r/m8
    instr!(parse_x0fb6, Opcode::Movzx, [0x0f, 0xb6], /r32, r/m8);

    // 0f b7 /r            => MOV r32, r/m16
    // REX.W + 0f b7 /r    => MOV r64, r/m16
    instr!(parse_x0fb7, Opcode::Movzx, [0x0f, 0xb7], /r32, r/m16);
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
    fn instr_mov_0fb7() {
        assert_eq!(
            Movzx::try_parse(b"\x0f\xb7\x45\xca", None),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Movzx,
                    op_1: Some(OpReg(eax())),
                    op_2: Some(OpMem(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: Some(rbp()),
                            index: None,
                            scale: None,
                            displacement: Some(Displacement::Byte(-54_i8))
                        }
                    })),
                    op_3: None,
                }
            )),
            "0f b7 45 ca     movzwl  -54(%rbp), %eax"
        );

        assert_eq!(
            Movzx::try_parse(b"\x0f\xb7\xc9", REX::new(0x44)),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Movzx,
                    op_1: Some(OpReg(r9d())),
                    op_2: Some(OpReg(cx())),
                    op_3: None,
                }
            )),
            "44 0f b7 c9     movzwl  %cx, %r9d"
        );
    }
}
