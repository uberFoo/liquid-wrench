use nom::*;

use crate::x86::instr::{DecodeInstruction, Instruction, Opcode, REX};

#[derive(Debug, PartialEq)]
pub(crate) struct Ret {}

impl DecodeInstruction for Ret {
    fn try_parse(input: &[u8], _rex: Option<REX>) -> IResult<&[u8], Instruction> {
        do_parse!(
            input,
            tag!(b"\xc3")
                >> (Instruction {
                    opcode: Opcode::Ret,
                    op_1: None,
                    op_2: None,
                    op_3: None
                })
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn instr_ret_c3() {
        assert_eq!(
            Ret::try_parse(b"\xc3", None),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Ret,
                    op_1: None,
                    op_2: None,
                    op_3: None
                }
            ))
        );
    }
}
