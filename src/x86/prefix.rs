use nom::*;

#[derive(Debug, PartialEq)]
enum Group1 {
    Lock = 0xf0,
    RepN = 0xf2,
    Rep = 0xf3,
}

#[derive(Debug, PartialEq)]
enum Group2 {
    CS = 0x2e,
    SS = 0x36,
    DS = 0x3e,
    ES = 0x26,
    FS = 0x64,
    GS = 0x65,
}

#[derive(Debug, PartialEq)]
enum Group3 {
    OperandSizeOverride = 0x66,
}

#[derive(Debug, PartialEq)]
enum Group4 {
    AddressSizeOverride = 0x67,
}

named!(
    group_one_prefix<&[u8], Option<Group1>>,
    opt!(
        alt!(
            value!(Group1::Lock, tag!([Group1::Lock as u8]))
                | value!(Group1::RepN, tag!([Group1::RepN as u8]))
                | value!(Group1::Rep, tag!([Group1::Rep as u8]))
        )
    )
);

named!(
    group_two_prefix<&[u8], Option<Group2>>,
    opt!(
        alt!(
            value!(Group2::CS, tag!([Group2::CS as u8]))
            | value!(Group2::SS, tag!([Group2::SS as u8]))
            | value!(Group2::DS, tag!([Group2::DS as u8]))
            | value!(Group2::ES, tag!([Group2::ES as u8]))
            | value!(Group2::FS, tag!([Group2::FS as u8]))
            | value!(Group2::GS, tag!([Group2::GS as u8]))
        )
    )
);

named!(
    group_three_prefix<&[u8], Option<Group3>>,
    opt!(value!(Group3::OperandSizeOverride, tag!([Group3::OperandSizeOverride as u8])))
);

named!(
    group_four_prefix<&[u8], Option<Group4>>,
    opt!(value!(Group4::AddressSizeOverride, tag!([Group4::AddressSizeOverride as u8])))
);

named!(
    prefixes<&[u8],
        // (Group1,
        //  Group2,
        //  Group3,
        //  Group4)>,
        (Option<Group1>,
         Option<Group2>,
         Option<Group3>,
         Option<Group4>)>,
    dbg_dmp!(
    permutation!(
        group_one_prefix,
        group_two_prefix,
        group_three_prefix,
        group_four_prefix))
);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn group_one() {
        assert_eq!(
            group_one_prefix(b"\xf0\x07"),
            Ok((&b"\x07"[..], Some(Group1::Lock)))
        );
    }

    #[test]
    fn all_macros() {
        assert_eq!(
            // prefixes(b"\x26\x67\x66\xf0"),
            prefixes(b"\xf0\x26\x66\x67"),
            Ok((
                &b""[..],
                (
                    Some(Group1::Lock),
                    Some(Group2::ES),
                    Some(Group3::OperandSizeOverride),
                    Some(Group4::AddressSizeOverride)
                )
            ))
        );
    }
}