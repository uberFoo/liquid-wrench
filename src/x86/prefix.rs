//! Prefix Bytes
//!
//! There is much goofiness herein. The prefix bytes may appear in any order, and we thus use nom's
//! `permutation!` macro. Not all bytes are however required, which means that we need to use
//! `permutation!`s `?` feature which optionally matches the rule. The problem is that the final
//! rule cannot have a `?` applied to it. (nom bug?)
//!
//! We can abuse the fact that a prefix byte always has an instruction following it. The final rule
//! in the `permutation!` call is a `tag!("")`, which doesn't consume a byte, but it does expect
//! there to be one. This results in a tuple with a bogus final element, which we get rid of in
//! the `Prefix` struct's constructor.
//!
use nom::*;

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) struct Prefix {
    group1: Option<Group1>,
    group2: Option<Group2>,
    group3: Option<Group3>,
    group4: Option<Group4>,
}

impl Prefix {
    pub(crate) fn new(
        groups: (
            Option<Group1>,
            Option<Group2>,
            Option<Group3>,
            Option<Group4>,
            &[u8],
        ),
    ) -> Self {
        Prefix {
            group1: groups.0,
            group2: groups.1,
            group3: groups.2,
            group4: groups.3,
        }
    }

    pub(crate) fn group1(&self) -> &Option<Group1> {
        &self.group1
    }

    pub(crate) fn group2(&self) -> &Option<Group2> {
        &self.group2
    }

    pub(crate) fn group3(&self) -> &Option<Group3> {
        &self.group3
    }

    pub(crate) fn group4(&self) -> &Option<Group4> {
        &self.group4
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum Group1 {
    Lock = 0xf0,
    RepN = 0xf2,
    Rep = 0xf3,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum Group2 {
    CS = 0x2e,
    SS = 0x36,
    DS = 0x3e,
    ES = 0x26,
    FS = 0x64,
    GS = 0x65,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum Group3 {
    OperandSizeOverride = 0x66,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum Group4 {
    AddressSizeOverride = 0x67,
}

named!(
    group_one_prefix<&[u8], Group1>,
        alt!(
            value!(Group1::Lock, tag!([Group1::Lock as u8]))
                | value!(Group1::RepN, tag!([Group1::RepN as u8]))
                | value!(Group1::Rep, tag!([Group1::Rep as u8]))
        )
);

named!(
    group_two_prefix<&[u8], Group2>,
        alt!(
            value!(Group2::CS, tag!([Group2::CS as u8]))
            | value!(Group2::SS, tag!([Group2::SS as u8]))
            | value!(Group2::DS, tag!([Group2::DS as u8]))
            | value!(Group2::ES, tag!([Group2::ES as u8]))
            | value!(Group2::FS, tag!([Group2::FS as u8]))
            | value!(Group2::GS, tag!([Group2::GS as u8]))
        )
);

named!(
    group_three_prefix<&[u8], Group3>,
    value!(Group3::OperandSizeOverride, tag!([Group3::OperandSizeOverride as u8]))
);

named!(
    group_four_prefix<&[u8], Group4>,
    value!(Group4::AddressSizeOverride, tag!([Group4::AddressSizeOverride as u8]))
);

named!( pub(crate)
    prefixes<&[u8],
    (Option<Group1>, Option<Group2>, Option<Group3>, Option<Group4>, &[u8])>,
    permutation!(
        group_one_prefix?,
        group_two_prefix?,
        group_three_prefix?,
        group_four_prefix?,
        tag!("")
    )
);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn group_one() {
        assert_eq!(
            group_one_prefix(b"\xf0\x07"),
            Ok((&b"\x07"[..], Group1::Lock))
        );
    }

    #[test]
    fn group_three() {
        assert_eq!(
            group_three_prefix(b"\x66\x07"),
            Ok((&b"\x07"[..], Group3::OperandSizeOverride))
        );
    }

    #[test]
    fn group_three_prefixes() {
        assert_eq!(
            prefixes(b"\x66\x08"),
            Ok((
                &b"\x08"[..],
                (
                    None,
                    None,
                    Some(Group3::OperandSizeOverride),
                    None,
                    &b""[..]
                )
            ),)
        );
    }

    #[test]
    fn group_one_and_three_reverse_order() {
        assert_eq!(
            prefixes(b"\x66\xf2\x08"),
            Ok((
                &b"\x08"[..],
                (
                    Some(Group1::RepN),
                    None,
                    Some(Group3::OperandSizeOverride),
                    None,
                    &b""[..]
                )
            ))
        );
    }

    #[test]
    fn all_macros() {
        assert_eq!(
            prefixes(b"\xf0\x26\x66\x67"),
            Ok((
                &b""[..],
                (
                    Some(Group1::Lock),
                    Some(Group2::ES),
                    Some(Group3::OperandSizeOverride),
                    Some(Group4::AddressSizeOverride),
                    &b""[..]
                )
            ))
        );
    }
}
