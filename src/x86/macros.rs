//! Who doesn't love a tasty Macro!?
//!

/// A macro to Generate [`Instruction`] Parsers
///
/// This macro generates a [`nom`] macro that can parse a particular X86 instruction encoding. Note
/// that it currently only decodes 64-bit instructions.
///
/// The format of the macro invocation is:
///
/// `instr!(parser_name, [opcode_bytes]+/extension_bits, operand_1, operand_2, ...)`,
///
/// where:
///  * `parser_name` is the name to which the generated parser should be bound, e.g., `parse_x20`,
/// or `parse_and`, etc.
///  * `[opcode_bytes]` is an array of `u8`s that identify the instruction opcode
///  * `+/extension_bits` is optional, and specifies the value of the opcode extension in the
/// `ModR/M` byte
///  * `operand_?` specifies how each operand, for the given opcode, is encoded
///
/// Valid operand encodings are specified as closely to the *Intel Reference* as possible. Valid
/// operands are currently:
///  * `/r8`, `/r32`, `/r64`
///  * `r/m8`, `r/m16`, `r/m32` `r/m64`
///  * `imm8`, `imm32`
///  * `m`
///  * `rel8`, `rel16`, `rel32`
///  * `reg:al`, `reg:eax`
///
/// # Examples
/// Parse the `And` instruction, encoded using `0x20`, to a function called `parse_x20`:
/// ```ignore
/// instr!(parse_x20, Opcode::And, [0x20], r/m8, /r8);
/// ```
///
/// # A Detailed Description of how the macro Works
/// There are three *@-prefixes* below.  The main entry point has no prefix, and first strips off
/// the name of the function to output and the type of the [`Instruction`] being parsed. These
/// are used to first output the function definition, and the macro calls itself recursively inside
/// the function body.
///
/// The function signature looks like this
///
/// `fn $name(input: &[u8], _prefix: PrefixBytes) -> IResult<&[u8], (Instruction)>`
///
/// The `input`, and `_prefix` parameters are passed along for use by subsequent macros. In fact a
/// lot of data generated by sub-rules is passed along to recursive calls, and it's all quite messy.
/// It is necessary for all of these variables to be kept in context to maintain macro hygiene.
///
/// The first recursive call begins by calling `@parse` rules. The have the following form:
///
/// `(@parse (input, _prefix), [$($args:tt)*], $($tail)*)`
///
/// The purpose of the `@parse` rules is to build up the input to `nom`s `do_parse!` macro, which
/// processes a sequence of nom-parsers, optionally storing the results of each, and returns a
/// tuple. We use `do_parse!` to first check for an `opcode` match, which includes optionally
/// verifying (but not removing from the input) optional *opcode-extension* bits in the `ModR/M`
/// byte.
///
/// After validating the opcode, we parse the operands for the instruction. Doing so may require
/// parsing a `ModR/M` byte, a SIB byte, and/or bytes for immediate values, or address
/// displacements. This is where it can get messy.
///
/// The operands parsing specifications come from the macro arguments. The first argument after
/// the opcode specification is the first operand, the second argument is the second operand, etc.
///
/// Above in our `@parse` rule was an `[$($args:tt)*]` macro parameter. We use `args` as a stack: as
/// operand arguments (from the macro invocation) are parsed, they are pushed on to the `args` stack
/// for later processing. This intermediate step allows us to determine operand dependencies prior
/// to actually parsing the operands. Note that storing args as a *token tree* is essential.  This
/// keeps Rust from complaining about illegal syntax, etc.
///
/// For example, `/r8` indicates that an eight-bit register is encoded in the `ModR/M` byte.  This
/// implies that we need to parse the `ModR/M` byte. However, we need to recognize the opcode
/// byte(s) first -- thus a LIFO of `args` that are processed later.
///
/// After all of the macro arguments are processed, we need to generate `nom` parsers, which occurs
/// in the `@nom` parser rules. Recall that at this point we have been carrying around with us some
/// variables that exist in the enclosing function scope. We also have a stack of things that need
/// parsing. We will use these to generate our `nom` parsers, which we will store in a list:
/// `$parsers`.
///
/// While a list of parsers is necessary, it is not sufficient. Variables used to store parsed
/// intermediates are created in the parser rules, and they must be available until they are
/// returned from `do_parse!`. Therefore, we must carry those around with us as we parse along our
/// merry way.
///
/// Here's the entire mess that are `@nom` parsers:
///
/// `(@nom ($($operands:expr)*), ($($params:expr), *), [$($args:tt)*], {$($parsers:tt)*})`
///
/// *Why on earth did I decide to shove the operands in front?*
/// *Why are the fn-level parameters comma-separated, but the operands are not?*
///
/// First note that `operands` must be expression *fragments*. This is so that hygiene works. In a
/// similar vein, the parsers must be token trees, but this is so that the `nom` macros can parse
/// them.
///
/// # Warning
/// *Just like any parser, the ordering of the rules is important.  Be careful...*
///
/// [`Instruction`]: crate::x86::instr::Instruction
#[macro_export]
macro_rules! instr {
    // Recognize `cb` -- an 8 bit relative address.
    (@parse ($($params:expr),*), [$($args:tt)*], ,rel8 $($tail:tt)*) => {
        instr! {
            @parse
            ($($params),*),
            [rel8, $($args)*],
            $($tail)*
        }
    };

    // Recognize `cw` -- a 16 bit relative address.
    (@parse ($($params:expr),*), [$($args:tt)*], ,rel16 $($tail:tt)*) => {
        instr! {
            @parse
            ($($params),*),
            [rel16, $($args)*],
            $($tail)*
        }
    };

    // Recognize `cd` -- a 32 bit relative address.
    (@parse ($($params:expr),*), [$($args:tt)*], ,rel32 $($tail:tt)*) => {
        instr! {
            @parse
            ($($params),*),
            [rel32, $($args)*],
            $($tail)*
        }
    };

    // Recognize `m` -- a memory operand.
    // This is, as far as I can tell, exactly the same behavior as r/m32
    // Can it appear only in last position?
    (@parse ($($params:expr),*), [$($args:tt)*], ,m $($tail:tt)*) => {
        instr! {
            @parse
            ($($params),*),
            [modrm, r/m32, $($args)*],
            $($tail)*
        }
    };

    // Recognize reg:eax
    // FIXME: This needs to be generalized.
    //
    (@parse ($($params:expr),*), [$($args:tt)*], ,reg:eax $($tail:tt)*) => {
        instr! {
            @parse
            ($($params),*),
            [reg:eax, $($args)*],
            $($tail)*
        }
    };

    // Recognize reg:al
    // FIXME: This needs to be generalized.
    //
    (@parse ($($params:expr),*), [$($args:tt)*], ,reg:al $($tail:tt)*) => {
        instr! {
            @parse
            ($($params),*),
            [reg:al, $($args)*],
            $($tail)*
        }
    };

    // Recognize /r8
    // NB the preceding comma
    (@parse ($($params:expr),*), [$($args:tt)*], ,/r8 $($tail:tt)*) => {
        instr! {
            @parse
            ($($params),*),
            [modrm, /r8, $($args)*],
            $($tail)*
        }
    };

    // Recognize /r32
    // NB the preceding comma
    (@parse ($($params:expr),*), [$($args:tt)*], ,/r32 $($tail:tt)*) => {
        instr! {
            @parse
            ($($params),*),
            [modrm, /r32, $($args)*],
            $($tail)*
        }
    };

    // Recognize /r64
    // NB the preceding comma
    (@parse ($($params:expr),*), [$($args:tt)*], ,/r64 $($tail:tt)*) => {
        instr! {
            @parse
            ($($params),*),
            [modrm, /r64, $($args)*],
            $($tail)*
        }
    };

    // Recognize r/m8
    // NB the preceding comma
    (@parse ($($params:expr),*), [$($args:tt)*], ,r/m8 $($tail:tt)*) => {
        instr! {
            @parse
            ($($params),*),
            [modrm, r/m8, $($args)*],
            $($tail)*
        }
    };

    // Recognize r/m16
    // NB the preceding comma
    (@parse ($($params:expr),*), [$($args:tt)*], ,r/m16 $($tail:tt)*) => {
        instr! {
            @parse
            ($($params),*),
            [modrm, r/m16, $($args)*],
            $($tail)*
        }
    };

    // Recognize r/m32
    // NB the preceding comma
    (@parse ($($params:expr),*), [$($args:tt)*], ,r/m32 $($tail:tt)*) => {
        instr! {
            @parse
            ($($params),*),
            [modrm, r/m32, $($args)*],
            $($tail)*
        }
    };

    // Recognize r/m32
    // NB the preceding comma
    (@parse ($($params:expr),*), [$($args:tt)*], ,r/m32 $($tail:tt)*) => {
        instr! {
            @parse
            ($($params),*),
            [modrm, r/m32, $($args)*],
            $($tail)*
        }
    };

    // Recognize r/m64
    // NB the preceding comma
    (@parse ($($params:expr),*), [$($args:tt)*], ,r/m64 $($tail:tt)*) => {
        instr! {
            @parse
            ($($params),*),
            [modrm, r/m64, $($args)*],
            $($tail)*
        }
    };

    // Recognize imm8
    // NB the preceding comma.
    (@parse ($($params:expr),*), [$($args:tt)*], ,imm8 $($tail:tt)*) => {
        instr! {
            @parse
            ($($params),*),
            [imm8, $($args)*],
            $($tail)*
        }
    };

    // Recognize imm32
    // NB the preceding comma.
    (@parse ($($params:expr),*), [$($args:tt)*], ,imm32 $($tail:tt)*) => {
        instr! {
            @parse
            ($($params),*),
            [imm32, $($args)*],
            $($tail)*
        }
    };

    // Recognize opcode bytes with extra bits in `ModR/M`
    // NB: This must come before the byte recognizer, below.
    (@parse ($($params:expr),*), [$($args:tt)*], +/ $ext:tt $($tail:tt)*) => {
        instr! {
            @parse
            ($($params),*),
            [ext: $ext, $($args)*],
            $($tail)*
        }
    };

    // Recognize opcode bytes
    // This should be the first pattern to be processed in the @parse phase.
    (@parse ($($params:expr),*), [$($args:tt)*], $bytes:tt $($tail:tt)*) => {
        instr! {
            @parse
            ($($params),*),
            [bytes: $bytes, $($args)*],
            $($tail)*
        }
    };

    // Start building nom parsers
    // This happens when there are no more token trees to parse.
    (@parse ($($params:expr),*), $args:tt,) => {
        instr! {
            @nom
            (),
            ($($params),*),
            $args,
            {}
        }
    };

    // Add the tag! parser for the opcode bytes.
    // NB: The parsers are generated from a stack of $args, so we must maintain that order when
    // building them.
    (@nom ($($oprnds:expr)*), ($($params:expr),*), [bytes: $bytes:expr, $($args:tt)*], {$($parsers:tt)*}) => {
        instr!{
            @nom
            ($($oprnds)*),
            ($($params),*),
            [$($args)*],
            {tag!($bytes) >> $($parsers)* }
        }
    };

    // Add parsing for checking the opcode extension bits.
    (@nom ($($oprnds:expr)*), ($($params:expr),*), [ext: $ext:expr, $($args:tt)*], {$($parsers:tt)*}) => {
        instr!{
            @nom
            ($($oprnds)*),
            ($($params),*),
            [$($args)*],
            {
                peek!(bits!(do_parse!(
                    take_bits!(u8, 2) >> tag_bits!(u8, 3, $ext) >> ()
                ))) >>
                $($parsers)*
            }
        }
    };

    // Add `ModR/M` parser
    (@nom ($($oprnds:expr)*), ($i:expr, $r:expr), [modrm, $($args:tt)*], {$($parsers:tt)*}) => (
        {
            use $crate::x86::modrm::ModRM;
            instr!{
                @nom
                ($($oprnds)*),
                ($i, $r, modrm),
                [$($args)*],
                {modrm: call!(ModRM::new, $r.rex()) >> $($parsers)* }
            }
        }
    );

    // But only once...
    (@nom ($($oprnds:expr)*), ($i:expr, $r:expr, $modrm:expr), [modrm, $($args:tt)*], {$($parsers:tt)*}) => {
        instr!{
            @nom
            ($($oprnds)*),
            ($i, $r, $modrm),
            [$($args)*],
            {$($parsers)*}
        }
    };

    (@nom ($($oprnds:expr)*), ($($params:expr),*), [rel8, $($args:tt)*], {$($parsers:tt)*}) => (
        {
            use $crate::x86::{instr::{
                Displacement, EffectiveAddress, LogicalAddress, Operand},
            };
            instr!(
                @nom
                (Operand::Memory(LogicalAddress {
                    segment: None,
                    offset: EffectiveAddress {
                        base: None,
                        index: None,
                        scale: None,
                        displacement: Some(Displacement::Byte(disp))
                    }
                }) $($oprnds)*),
                ($($params),*),
                [$($args)*],
                {disp: le_i8 >> $($parsers)*}
            )
        }
    );

    (@nom ($($oprnds:expr)*), ($($params:expr),*), [rel16, $($args:tt)*], {$($parsers:tt)*}) => (
        {
            use $crate::x86::{instr::{
                Displacement, EffectiveAddress, LogicalAddress, Operand},
            };
            instr!(
                @nom
                (Operand::Memory(LogicalAddress {
                    segment: None,
                    offset: EffectiveAddress {
                        base: None,
                        index: None,
                        scale: None,
                        displacement: Some(Displacement::Word(disp))
                    }
                }) $($oprnds)*),
                ($($params),*),
                [$($args)*],
                {disp: le_i16 >> $($parsers)*}
            )
        }
    );

    (@nom ($($oprnds:expr)*), ($($params:expr),*), [rel32, $($args:tt)*], {$($parsers:tt)*}) => (
        {
            use $crate::x86::{instr::{
                Displacement, EffectiveAddress, LogicalAddress, Operand},
            };
            instr!(
                @nom
                (Operand::Memory(LogicalAddress {
                    segment: None,
                    offset: EffectiveAddress {
                        base: None,
                        index: None,
                        scale: None,
                        displacement: Some(Displacement::DWord(disp))
                    }
                }) $($oprnds)*),
                ($($params),*),
                [$($args)*],
                {disp: le_i32 >> $($parsers)*}
            )
        }
    );

    // Parse reg:eax
    // FIXME: Need a better way to deal with register width
    (@nom ($($oprnds:expr)*), ($($params:expr),*), [reg:eax, $($args:tt)*], {$($parsers:tt)*}) => (
        {
            use $crate::x86::{instr::Operand, register::ctors::eax};
            instr!{
                @nom
                (Operand::Register(eax()) $($oprnds)*),
                ($($params),*),
                [$($args)*],
                {$($parsers)*}
            }
        }
    );

    // Parse reg:al
    // FIXME: Need a better way to deal with register width
    (@nom ($($oprnds:expr)*), ($($params:expr),*), [reg:al, $($args:tt)*], {$($parsers:tt)*}) => (
        {
            use $crate::x86::{instr::Operand, register::ctors::al};
            instr!{
                @nom
                (Operand::Register(al()) $($oprnds)*),
                ($($params),*),
                [$($args)*],
                {$($parsers)*}
            }
        }
    );

    // Parse /r8
    (@nom ($($oprnds:expr)*), ($i:expr, $r:expr, $modrm:expr), [/r8, $($args:tt)*], {$($parsers:tt)*}) => {
        instr!{
            @nom
            ($modrm.r8() $($oprnds)*),
            ($i, $r, $modrm),
            [$($args)*],
            {$($parsers)*}
        }
    };

    // Parse /r32
    (@nom ($($oprnds:expr)*), ($i:expr, $r:expr, $modrm:expr), [/r32, $($args:tt)*], {$($parsers:tt)*}) => {
        instr!{
            @nom
            ($modrm.r32() $($oprnds)*),
            ($i, $r, $modrm),
            [$($args)*],
            {$($parsers)*}
        }
    };

    // Parse /r64
    (@nom ($($oprnds:expr)*), ($i:expr, $r:expr, $modrm:expr), [/r64, $($args:tt)*], {$($parsers:tt)*}) => {
        instr!{
            @nom
            ($modrm.r64() $($oprnds)*),
            ($i, $r, $modrm),
            [$($args)*],
            {$($parsers)*}
        }
    };

    // Parse r/m8
    (@nom ($($oprnds:expr)*), ($i:expr, $r:expr, $modrm:expr), [r/m8, $($args:tt)*], {$($parsers:tt)*}) => {
        instr!{
            @nom
            ($modrm.r_m8() $($oprnds)*),
            ($i, $r, $modrm),
            [$($args)*],
            {$($parsers)*}
        }
    };

    // Parse r/m16
    (@nom ($($oprnds:expr)*), ($i:expr, $r:expr, $modrm:expr), [r/m16, $($args:tt)*], {$($parsers:tt)*}) => {
        instr!{
            @nom
            ($modrm.r_m16() $($oprnds)*),
            ($i, $r, $modrm),
            [$($args)*],
            {$($parsers)*}
        }
    };

    // Parse r/m32
    (@nom ($($oprnds:expr)*), ($i:expr, $r:expr, $modrm:expr), [r/m32, $($args:tt)*], {$($parsers:tt)*}) => {
        instr!{
            @nom
            ($modrm.r_m32() $($oprnds)*),
            ($i, $r, $modrm),
            [$($args)*],
            {$($parsers)*}
        }
    };

    // Parse r/m64
    (@nom ($($oprnds:expr)*), ($i:expr, $r:expr, $modrm:expr), [r/m64, $($args:tt)*], {$($parsers:tt)*}) => {
        instr!{
            @nom
            ($modrm.r_m64() $($oprnds)*),
            ($i, $r, $modrm),
            [$($args)*],
            {$($parsers)*}
        }
    };

    // Parse out the immediate
    (@nom ($($oprnds:expr)*), ($($params:expr),*), [imm8, $($args:tt)*], {$($parsers:tt)*}) => (
        {
            use $crate::x86::{Width, instr::ImmediateBuilder};
            instr!{
                @nom
                (ImmediateBuilder::new(Width::Byte).signed(imm) $($oprnds)*),
                ($($params),*),
                [$($args)*],
                {imm: le_i8 >> $($parsers)* }
            }
        }
    );

    // Parse out the immediate
    (@nom ($($oprnds:expr)*), ($($params:expr),*), [imm32, $($args:tt)*], {$($parsers:tt)*}) => (
        {
            use $crate::x86::{Width, instr::ImmediateBuilder};
            instr!{
                @nom
                (ImmediateBuilder::new(Width::DWord).signed(imm) $($oprnds)*),
                ($($params),*),
                [$($args)*],
                {imm: le_i32 >> $($parsers)* }
            }
        }
    );


    // Finally pass the mess on to build the nom parser.
    // NB that we are stripping off the $modrm expression, as it's no longer needed.
    (@nom ($($oprnds:expr)*), ($i:expr, $r:expr $(, $modrm:expr)?), [], $parsers:tt) => {
        instr!{
            @output
            ($($oprnds),*),
            ($i, $r),
            $parsers
        }
    };

    // So for this is special for NOP
    (@output (), ($i:expr, $r:expr), {$($parsers:tt)*}) => {
        {
            do_parse!{
                $i,
                $($parsers)*
                (None, None, None)
            }
        }
    };

    // The chicken.
    // Parse an instruction that has one operand.
    // Note that $i is the input slice, which must be passed as the first argument to `do_parse!`.
    (@output ($op_1:expr), ($i:expr, $r:expr), {$($parsers:tt)*}) => {
        {
            trace_macros!(false);
            do_parse!{
                $i,
                $($parsers)*
                (Some($op_1), None, None)
            }
        }
    };

    // The chicken.
    // Parse an instruction that has two operands.
    // Note that $i is the input slice, which must be passed as the first argument to `do_parse!`.
    (@output ($op_1:expr, $op_2: expr), ($i:expr, $r:expr), {$($parsers:tt)*}) => {
        {
            trace_macros!(false);
            do_parse!{
                $i,
                $($parsers)*
                (Some($op_1), Some($op_2), None)
            }
        }
    };

    // The chicken.
    // Parse an instruction that has three operands.
    // Note that $i is the input slice, which must be passed as the first argument to `do_parse!`.
    (@output ($op_1:expr, $op_2: expr, $op_3:expr), ($i:expr, $r:expr), {$($parsers:tt)*}) => {
        {
            trace_macros!(false);
            do_parse!{
                $i,
                $($parsers)*
                (Some($op_1), Some($op_2), Some($op_3))
            }
        }
    };

    // Main entry point.
    ($name:ident, $inst:expr, $($tail:tt)*) => (
        fn $name(input: &[u8], _prefix: PrefixBytes) -> IResult<&[u8], (Instruction)> {
            use $crate::x86::Width;
            trace_macros!(false);
            let parsed = instr!{
                @parse
                (input, _prefix),
                [],
                $($tail)*
            };
            match parsed {
                Ok((rest, operands)) => {
                    Ok((rest, Instruction {
                        width: Width::QWord,
                        opcode: $inst,
                        op_1: operands.0,
                        op_2: operands.1,
                        op_3: operands.2
                    }))},
                Err(e) => Err(e)
            }
        }
    );
}
