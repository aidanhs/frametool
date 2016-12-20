#![feature(plugin,range_contains,trace_macros)]
#![plugin(interpolate_idents)]
extern crate either;
#[macro_use]
extern crate nom;
extern crate regex;

use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;
use std::iter;
use std::mem;
use std::str;
use std::usize;

use regex::Regex;

const MIF: &'static str = "/media/aidanhs/OS/Users/aidanhs/Desktop/ch01__.mif";
const FM: &'static str =  "/media/aidanhs/OS/Users/aidanhs/Desktop/ch01__.fm";

fn main() {
    mif2asciidoc()
}

fn mif2asciidoc() {
    let mut mifvec = vec![];
    //File::open(MIF).unwrap().read_to_end(&mut mifvec).unwrap();
    File::open("y.mif").unwrap().read_to_end(&mut mifvec).unwrap();

    //let mifstr = str::from_utf8(&mifvec).unwrap();
    parser::parse(&mifvec)
}

mod parser {
    #![allow(non_snake_case)]

    use std::ops::Range;
    use std::str;
    use std::str::FromStr;

    use either::Either;

    use nom::Err;
    use nom::ErrorKind;
    use nom::IResult;
    use nom::alpha;
    use nom::alphanumeric;
    use nom::anychar;
    use nom::digit;
    use nom::eol;
    use nom::is_digit;
    use nom::multispace;
    use nom::non_empty;
    use nom::not_line_ending;
    use nom::whitespace::sp;

    #[derive(Debug)]
    enum MIFTree<'a> {
        MIFFile(&'a str),
        Units(MIFUnit),
        CharUnits(MIFCharUnit),

        ColorCatalog(Vec<MIFTree<'a>>),
        Color(Vec<MIFTree<'a>>),
        ColorTag(&'a str),
        ColorCyan(f64),
        ColorMagenta(f64),
        ColorYellow(f64),
        ColorBlack(f64),
        ColorLibraryFamilyName(&'a str),
        ColorLibraryInkName(&'a str),
        ColorAttribute(MIFColorAttributeKeyword),
        ColorTint(f64),
        ColorTintBaseColor(&'a str),
        ColorOverprint(bool),

        ConditionCatalog(Vec<MIFTree<'a>>),
        Condition(Vec<MIFTree<'a>>),
        CTag(&'a str),
        CState(MIFCStateKeyword),
        CStyle(MIFCStyleKeyword),
        CColor(&'a str),
        CSeparation(u64),
        CBackgroundColor(&'a str),

        BoolCondCatalog(Vec<MIFTree<'a>>),
        BoolCond(Vec<MIFTree<'a>>),
        BoolCondTag(&'a str),
        BoolCondExpr(&'a str),
        // MIFNOTE: undocumented
        BoolCondExprName(&'a str),
        BoolCondState(&'a str),

        DefAttrValuesCatalog(Vec<MIFTree<'a>>),
        DefAttrValues(Vec<MIFTree<'a>>),
        AttributeTag(&'a str),
        AttributeValue(&'a str),

        AttrCondExprCatalog(Vec<MIFTree<'a>>),
        AttrCondExpr(Vec<MIFTree<'a>>),
        AttrCondExprTag(&'a str),
        AttrCondExprStr(&'a str),
        AttrCondState(&'a str),

        DictionaryPreferences(Box<MIFTree<'a>>),
        DiLanguages(Vec<MIFTree<'a>>),
        DiLanguage(MIFDiLanguage),
        DiService(Box<MIFTree<'a>>, Box<MIFTree<'a>>),
        DiSpellProvider(&'a str),
        DiHyphenProvider(&'a str),

        CombinedFontCatalog,
    }

    enum MIFErr {
        OutOfRangeError,
    }

    #[derive(Debug)]
    enum MIFUnit {
        Centimeters,
        Ciceros,
        Didots,
        Inches,
        Millimeters,
        Picas,
        Points,
    }

    #[derive(Debug)]
    enum MIFCharUnit {
        Points,
        Q,
    }

    #[derive(Debug)]
    enum MIFColorAttributeKeyword {
        ColorIsBlack,
        ColorIsWhite,
        ColorIsRed,
        ColorIsGreen,
        ColorIsBlue,
        ColorIsCyan,
        ColorIsMagenta,
        ColorIsYellow,
        ColorIsDarkGrey,
        ColorIsPaleGreen,
        ColorIsForestGreen,
        ColorIsRoyalBlue,
        ColorIsMauve,
        ColorIsLightSalmon,
        ColorIsOlive,
        ColorIsSalmon,
        ColorIsReserved,

        // MIFNOTE: Undocumented
        ColorIsDarkYellow,
    }

    #[derive(Debug)]
    enum MIFCStateKeyword {
        CHidden,
        CShown,
    }

    #[derive(Debug)]
    enum MIFCStyleKeyword {
        CAsIs,
        CUnderline,
        CDoubleUnderline,
        CStrike,
        COverline,
        CChangeBar,
    }

    // TODO: probably incomplete
    #[derive(Debug)]
    enum MIFDiLanguage {
        USEnglish,
        UKEnglish,
    }

    pub fn parse(mif: &[u8]) {
        match fullmiffile(mif) {
            IResult::Done(unparsed, miftree) => {
                println!("DONE: {:?}\n\n\nunparsed:{:?}", miftree, str::from_utf8(unparsed).unwrap());
            },
            IResult::Error(e) => {
                println!("ERROR:\n");
                let mut next = e;
                loop {
                    next = match next {
                        Err::Code(e) => {
                            println!("CODE: {:?}\n", e);
                            break
                        },
                        Err::Node(e, next) => {
                            println!("NODE: {:?}\n", e);
                            *next
                        },
                        Err::Position(e, p) => {
                            println!("POSITION: {:?} {:?}\n", e, &str::from_utf8(p).unwrap()[..100]);
                            break
                        },
                        Err::NodePosition(e, p, next) => {
                            println!("NODEPOSITION: {:?} {:?}\n", e, &str::from_utf8(p).unwrap()[..100]);
                            *next
                        },
                    }
                }
            },
            IResult::Incomplete(needed) => {
                println!("NEEDED: {:?}", needed)
            },
        }
    }

    fn to_s(b: Vec<u8>) -> String {
        String::from_utf8(b).unwrap()
    }
    fn checkrange(i: u64, r: Range<u64>) -> Result<u64, MIFErr> {
        if r.contains(i) { Ok(i) } else { Err(MIFErr::OutOfRangeError) }
    }

    //// `\xnn` -> u16
    //// note that additional conversion needs to be done since the framemaker
    //// charset is a bit odd
    //named!(data_string_byte_escape <u16>,
    //    map_res!(
    //        do_parse!(
    //            tag!("\\x") >>
    //            bytes: take!(2) >>
    //            tag!(" ") >>
    //            (bytes)
    //        ),
    //        |bytes| {
    //            match str::from_utf8(bytes).map(|s| u16::from_str_radix(s, 16)) {
    //                Ok(Ok(n)) => Ok(n),
    //                Ok(Err(parseinterr)) => Err(Either::Left(parseinterr)),
    //                Err(utf8err) => Err(Either::Right(utf8err)),
    //            }
    //        }
    //    )
    //);
    //named!(data_string <String>,
    //  map!(
    //    escaped_transform!(call!(alpha), '\\',
    //      alt!(
    //          tag!("t")       => { |_| &b"\t"[..] }
    //        | tag!(">")       => { |_| &b">"[..] }
    //        | tag!("q")       => { |_| &b"'"[..] }
    //        | tag!("Q")       => { |_| &b"`"[..] }
    //        | tag!("\\")      => { |_| &b"\\"[..] }
    //        | tag!("\"")      => { |_| &b"\""[..] }
    //        | tag!("\"")      => { |_| &b"\""[..] }
    //        | data_string_byte_escape => { |n: u16| n.to_string().as_bytes() }
    //      )
    //    ), to_s
    //  )
    //);

    fn fail<I, O>(_: I) -> IResult<I, O> {
        IResult::Error(error_code!(ErrorKind::Custom(8965)))
    }

    named!(mif_parse_decimal<f64>,
        map_res!(
            recognize!(
                alt_complete!(
                    delimited!(digit, tag!("."), digit)
                  | digit
                )
            ),
            |bytes| {
                match str::from_utf8(bytes).map(|s| f64::from_str(s)) {
                    Ok(Ok(n)) => Ok(n),
                    Ok(Err(parsefloaterr)) => Err(Either::Left(parsefloaterr)),
                    Err(utf8err) => Err(Either::Right(utf8err)),
                }
            }
        )
    );
    named!(mif_parse_integer<u64>,
        map_res!(digit, |bytes| {
            match str::from_utf8(bytes).map(|s| u64::from_str_radix(s, 10)) {
                Ok(Ok(n)) => Ok(n),
                Ok(Err(parseinterr)) => Err(Either::Left(parseinterr)),
                Err(utf8err) => Err(Either::Right(utf8err)),
            }
        })
    );
    named!(mif_parse_string<&str>,
        do_parse!(
            tag!(L_QUOTE) >>
            s: map_res!(take_until!(R_QUOTE), str::from_utf8) >>
            tag!(R_QUOTE) >>
            (s)
        )
    );
    named!(mif_parse_charunit<MIFCharUnit>,
        do_parse!(tag!(b"CU") >> unit: alt_complete!(
            tag!(b"pt") => { |_| MIFCharUnit::Points }
          | tag!(b"Q") =>  { |_| MIFCharUnit::Q }
        )>> (unit))
    );
    named!(mif_parse_Uunit<MIFUnit>,
        do_parse!(tag!(b"U") >> unit: mif_parse_unit >> (unit))
    );
    named!(mif_parse_unit<MIFUnit>,
        alt_complete!(
            tag!(b"cm") =>     { |_| MIFUnit::Centimeters }
          | tag!(b"cicero") => { |_| MIFUnit::Ciceros }
          | tag!(b"cc") =>     { |_| MIFUnit::Ciceros }
          | tag!(b"dd") =>     { |_| MIFUnit::Didots }
          | tag!(b"in") =>     { |_| MIFUnit::Inches }
          | tag!(b"\"") =>     { |_| MIFUnit::Inches }
          | tag!(b"mm") =>     { |_| MIFUnit::Millimeters }
          | tag!(b"pica") =>   { |_| MIFUnit::Picas }
          | tag!(b"pi") =>     { |_| MIFUnit::Picas }
          | tag!(b"pc") =>     { |_| MIFUnit::Picas }
          | tag!(b"point") =>  { |_| MIFUnit::Points }
          | tag!(b"pt") =>     { |_| MIFUnit::Points }
        )
    );
    named!(mif_parse_colorattributekeyword<MIFColorAttributeKeyword>,
        alt_complete!(
            tag!(b"ColorIsBlack") =>       { |_| MIFColorAttributeKeyword::ColorIsBlack }
          | tag!(b"ColorIsWhite") =>       { |_| MIFColorAttributeKeyword::ColorIsWhite }
          | tag!(b"ColorIsRed") =>         { |_| MIFColorAttributeKeyword::ColorIsRed }
          | tag!(b"ColorIsGreen") =>       { |_| MIFColorAttributeKeyword::ColorIsGreen }
          | tag!(b"ColorIsBlue") =>        { |_| MIFColorAttributeKeyword::ColorIsBlue }
          | tag!(b"ColorIsCyan") =>        { |_| MIFColorAttributeKeyword::ColorIsCyan }
          | tag!(b"ColorIsMagenta") =>     { |_| MIFColorAttributeKeyword::ColorIsMagenta }
          | tag!(b"ColorIsYellow") =>      { |_| MIFColorAttributeKeyword::ColorIsYellow }
          | tag!(b"ColorIsDarkGrey") =>    { |_| MIFColorAttributeKeyword::ColorIsDarkGrey }
          | tag!(b"ColorIsPaleGreen") =>   { |_| MIFColorAttributeKeyword::ColorIsPaleGreen }
          | tag!(b"ColorIsForestGreen") => { |_| MIFColorAttributeKeyword::ColorIsForestGreen }
          | tag!(b"ColorIsRoyalBlue") =>   { |_| MIFColorAttributeKeyword::ColorIsRoyalBlue }
          | tag!(b"ColorIsMauve") =>       { |_| MIFColorAttributeKeyword::ColorIsMauve }
          | tag!(b"ColorIsLightSalmon") => { |_| MIFColorAttributeKeyword::ColorIsLightSalmon }
          | tag!(b"ColorIsOlive") =>       { |_| MIFColorAttributeKeyword::ColorIsOlive }
          | tag!(b"ColorIsSalmon") =>      { |_| MIFColorAttributeKeyword::ColorIsSalmon }
          | tag!(b"ColorIsReserved") =>    { |_| MIFColorAttributeKeyword::ColorIsReserved }

          | tag!(b"ColorIsDarkYellow") =>  { |_| MIFColorAttributeKeyword::ColorIsDarkYellow }
        )
    );
    named!(mif_parse_cstatekeyword<MIFCStateKeyword>,
        alt_complete!(
            tag!(b"CHidden") => { |_| MIFCStateKeyword::CHidden }
          | tag!(b"CShown") =>  { |_| MIFCStateKeyword::CShown }
        )
    );
    named!(mif_parse_cstylekeyword<MIFCStyleKeyword>,
        alt_complete!(
            tag!(b"CAsIs") =>            { |_| MIFCStyleKeyword::CAsIs }
          | tag!(b"CUnderline") =>       { |_| MIFCStyleKeyword::CUnderline }
          | tag!(b"CDoubleUnderline") => { |_| MIFCStyleKeyword::CDoubleUnderline }
          | tag!(b"CStrike") =>          { |_| MIFCStyleKeyword::CStrike }
          | tag!(b"COverline") =>        { |_| MIFCStyleKeyword::COverline }
          | tag!(b"CChangeBar") =>       { |_| MIFCStyleKeyword::CChangeBar }
        )
    );
    named!(mif_parse_dilanguage<MIFDiLanguage>,
        alt_complete!(
            tag!(b"USEnglish") => { |_| MIFDiLanguage::USEnglish }
          | tag!(b"UKEnglish") => { |_| MIFDiLanguage::UKEnglish }
        )
    );

    const L_QUOTE: &'static [u8] = b"\x60"; // '
    const R_QUOTE: &'static [u8] = b"\x27"; // `
    named!(data_string<&str>, call!(mif_parse_string));
    named!(data_tagstring<&str>, call!(mif_parse_string));
    // TODO: don't just return the untransformed value
    named!(data_path<&str>, call!(mif_parse_string));
    named!(data_boolean<bool>,
        alt!(
            tag!(b"Yes") => { |_| true }
          | tag!(b"No") =>  { |_| false }
        )
    );
    named!(data_integer<u64>, call!(mif_parse_integer));
    named!(data_ID<u64>,
        map_res!(mif_parse_integer, |i| checkrange(i, 1..65535+1))
    );
    // TODO: don't just return the untransformed value
    named!(data_dimension<&[u8]>,
        // TODO
        call!(fail)
    );
    named!(data_degrees<f64>, call!(mif_parse_decimal));
    named!(data_percentage<f64>, call!(mif_parse_decimal));
    // TODO: don't just return the untransformed value
    named!(data_metric<&[u8]>,
        // TODO
        call!(fail)
    );
    // TODO: don't just return the untransformed value
    named!(data_W_H<&[u8]>,
        // TODO
        call!(fail)
    );
    // TODO: don't just return the untransformed value
    named!(data_X_Y<&[u8]>,
        // TODO
        call!(fail)
    );
    // TODO: don't just return the untransformed value
    named!(data_L_T_R_B<&[u8]>,
        // TODO
        call!(fail)
    );
    // TODO: don't just return the untransformed value
    named!(data_L_T_W_H<&[u8]>,
        // TODO
        call!(fail)
    );
    // TODO: don't just return the untransformed value
    named!(data_X_Y_W_H<&[u8]>,
        // TODO
        call!(fail)
    );

    named!(maybe_spaces_and_comments<&[u8],&[u8]>,
        recognize!(fold_many1!(
            alt_complete!(
                do_parse!(
                    tag!(b"#") >> not_line_ending >> eol >>
                    (&[][..])
                )
              | sp // Returns success even when nothing consumed
            ),
            (), |_acc: (), _item| ()
        ))
    );
    named!(spaces_and_comments<&[u8],&[u8]>,
        recognize!(do_parse!(
            multispace >>
            maybe_spaces_and_comments >>
            (())
        ))
    );
    macro_rules! __st_check (
        ($name:ident) => (
            interpolate_idents! {
                named!([check_ $name]<()>,
                    peek!(
                        do_parse!(
                            tag!(b"<") >>
                            maybe_spaces_and_comments >>
                            tag!(stringify!($name)) >>
                            spaces_and_comments >>
                            (())
                        )
                    )
                );
            }
        )
    );
    macro_rules! __st_parse (
        ($name:ident, ($( $parsepart:tt )*)) => (
            interpolate_idents! {
                named!([statement_ $name]<MIFTree>,
                    delimited!(
                        do_parse!(
                            tag!(b"<") >>
                            maybe_spaces_and_comments >>
                            tag!(stringify!($name)) >>
                            spaces_and_comments >>
                            (())
                        ),
                        // TODO: sep intersperses before and after, as well
                        // as in-between. We just want in-between, so we have
                        // to use maybe_ for now. Though I'm not sure how using
                        // non-maybe_ would work for sub-trees - perhaps they
                        // don't need spaces between them
                        sep!(maybe_spaces_and_comments, do_parse!(
                            $( $parsepart )*
                        )),
                        tag!(b">")
                    )
                );
            }
        );
    );
    macro_rules! st (
        // Empty
        ($name:ident, (), ()) => (
            __st_check!($name);
            __st_parse!($name, ((MIFTree::$name)));
        );
        ($name:ident, ($( $part:tt )*), ($( $part2:tt )*)) => (
            __st_check!($name);
            __st_parse!($name, ($( $part )* >> (MIFTree::$name($( $part2 )*))));
        );
    );
    macro_rules! stparse (
        ($i:expr, $name:ident) => (
            interpolate_idents! {
                do_parse!($i,
                    [check_ $name] >>
                    x: return_error!(ErrorKind::Custom(589), [statement_ $name]) >>
                    (x)
                )
            }
        )
    );

    fn isfloatchr(b: u8) -> bool { b == b'.' || (b >= b'0' && b <= b'9') }
    st!(MIFFile, (vsn: take_while1!(isfloatchr)), (str::from_utf8(vsn).unwrap()));
    st!(Units, (unit: mif_parse_Uunit), (unit));
    st!(CharUnits, (charunit: mif_parse_charunit), (charunit));

    st!(ColorCatalog, (colors: many0!(stparse!(Color))), (colors));
    st!(Color,
        (props: many0!(alt_complete!(
            stparse!(ColorTag)
          | stparse!(ColorCyan)
          | stparse!(ColorMagenta)
          | stparse!(ColorYellow)
          | stparse!(ColorBlack)
          | stparse!(ColorLibraryFamilyName)
          | stparse!(ColorLibraryInkName)
          | stparse!(ColorAttribute)
          | stparse!(ColorTint)
          | stparse!(ColorTintBaseColor)
          | stparse!(ColorOverprint)
        ))),
        (props)
    );
    st!(ColorTag, (tag: data_tagstring), (tag));
    st!(ColorCyan, (pct: data_percentage), (pct));
    st!(ColorMagenta, (pct: data_percentage), (pct));
    st!(ColorYellow, (pct: data_percentage), (pct));
    st!(ColorBlack, (pct: data_percentage), (pct));
    st!(ColorLibraryFamilyName, (s: data_string), (s));
    st!(ColorLibraryInkName, (s: data_string), (s));
    st!(ColorAttribute, (a: mif_parse_colorattributekeyword), (a));
    st!(ColorTint, (pct: data_percentage), (pct));
    st!(ColorTintBaseColor, (s: data_string), (s));
    st!(ColorOverprint, (b: data_boolean), (b));

    st!(ConditionCatalog, (conds: many0!(stparse!(Condition))), (conds));
    st!(Condition,
        (props: many0!(alt_complete!(
            stparse!(CTag)
          | stparse!(CState)
          | stparse!(CStyle)
          | stparse!(CColor)
          | stparse!(CSeparation)
          | stparse!(CBackgroundColor)
        ))),
        (props)
    );
    st!(CTag, (tag: data_string), (tag));
    st!(CState, (keyword: mif_parse_cstatekeyword), (keyword));
    st!(CStyle, (keyword: mif_parse_cstylekeyword), (keyword));
    st!(CColor, (tag: data_tagstring), (tag));
    st!(CSeparation, (sep: data_integer), (sep));
    st!(CBackgroundColor, (tag: data_tagstring), (tag));

    st!(BoolCondCatalog, (conds: many0!(stparse!(BoolCond))), (conds));
    st!(BoolCond,
        (props: many0!(alt_complete!(
            stparse!(BoolCondTag)
          | stparse!(BoolCondExprName)
          | stparse!(BoolCondExpr)
          | stparse!(BoolCondState)
        ))),
        (props)
    );
    st!(BoolCondTag, (tag: data_string), (tag));
    st!(BoolCondExprName, (name: data_string), (name));
    // TODO: parse this properly
    st!(BoolCondExpr, (expr: data_string), (expr));
    // TODO: validate either Active or Inactive
    st!(BoolCondState, (state: data_string), (state));

    st!(DefAttrValuesCatalog, (attrvals: many0!(stparse!(DefAttrValues))), (attrvals));
    st!(DefAttrValues,
        (attrvalues: many0!(alt_complete!(
            stparse!(AttributeTag)
          | stparse!(AttributeValue)
        ))),
        (attrvalues)
    );
    st!(AttributeTag, (tag: data_string), (tag));
    st!(AttributeValue, (value: data_string), (value));

    st!(AttrCondExprCatalog, (attrvals: many0!(stparse!(AttrCondExpr))), (attrvals));
    st!(AttrCondExpr,
        (attrvalues: many0!(alt_complete!(
            stparse!(AttrCondExprTag)
          | stparse!(AttrCondExprStr)
          | stparse!(AttrCondState)
        ))),
        (attrvalues)
    );
    st!(AttrCondExprTag, (tag: data_string), (tag));
    st!(AttrCondExprStr, (str: data_string), (str));
    // TODO: validate either Active or Inactive, like BoolCondState
    st!(AttrCondState, (state: data_string), (state));

    // MIFNOTE: this is documented as 'Dictionary', which seems wrong?
    st!(DictionaryPreferences, (dil: stparse!(DiLanguages)), (Box::new(dil)));
    // TODO: each service follows a lang
    st!(DiLanguages,
        (langs: many0!(alt_complete!(
            stparse!(DiLanguage)
          | stparse!(DiService)
        ))),
        (langs)
    );
    // MIFNOTE: this is documented as a string, but doesn't seem to be
    st!(DiLanguage, (lang: mif_parse_dilanguage), (lang));
    // TODO: unsure if these are always in this order
    st!(DiService, (
            sp: stparse!(DiSpellProvider) >>
            hp: stparse!(DiHyphenProvider)
        ), (Box::new(sp), Box::new(hp))
    );
    // TODO: only two permitted strings here
    st!(DiSpellProvider, (provider: data_string), (provider));
    // TODO: only two permitted strings here
    st!(DiHyphenProvider, (provider: data_string), (provider));

    // TODO
    st!(CombinedFontCatalog, (), ());

    named!(fullmiffile<Vec<MIFTree>>,
        many0!(sep!(maybe_spaces_and_comments, alt_complete!(
            stparse!(MIFFile)
          | stparse!(Units)
          | stparse!(CharUnits)
          | stparse!(ColorCatalog)
          | stparse!(ConditionCatalog)
          | stparse!(BoolCondCatalog)
          | stparse!(DefAttrValuesCatalog)
          | stparse!(AttrCondExprCatalog)
          | stparse!(DictionaryPreferences)
          | stparse!(CombinedFontCatalog)
        )))
    );
}

// Attempt at a handwritten parser for the text format
//fn isspace(b: u8) -> bool {
//    b == b' ' || b == b'\n' || b == b'\r'
//}
//fn isrefspace(&b: &u8) -> bool { isspace(b) }
//
//fn chompspaces(s: &[u8]) -> &[u8] {
//    let pos = s.iter().position(|&b| !isspace(b)).unwrap_or_else(|| s.len());
//    &s[pos..]
//}
//
//fn parse(mut s: &[u8]) -> (&[u8], Vec<MifTree>) {
//    let mut acc = vec![];
//    loop {
//        println!("{:?}", str::from_utf8(&s[..15]).unwrap());
//        s = chompspaces(s);
//        match s.first() {
//            Some(&b'<') => s = &s[1..],
//            Some(&b'>') |
//            None => break,
//            Some(&b'#') => {
//                let newline = s.iter().position(|&b| b == b'\n').unwrap();
//                s = &s[newline+1..];
//                continue
//            },
//            Some(&b) => {
//                println!("Unknown byte {} ({:?})", b, char::from(b));
//                panic!()
//            },
//        }
//        s = chompspaces(s);
//            let tagend = s.iter().position(isrefspace).unwrap();
//        assert!(tagend > 0);
//            let tag = String::from_utf8(s[..tagend].to_owned()).unwrap();
//            s = &s[tagend..];
//        s = chompspaces(s);
//        loop {
//            let miftree = if s[0] == b'<' {
//                let (news, subtree) = parse(s);
//                s = news;
//                MifTree::Parent(tag, subtree)
//            } else {
//                let mut pos = 0;
//                while s[pos] != b'>' {
//                    assert!(s[pos] != b'<');
//                    pos += 1
//                }
//                let miftree = MifTree::Leaf(tag, s[..pos].to_owned());
//                s = &s[pos..];
//                miftree
//            };
//        }
//        assert!(s[0] == b'>');
//        s = &s[1..];
//    }
//    (s, acc)
//}

// Attempt to understand the binary format
//fn main2() {
//    let mut mifvec = vec![];
//    File::open(MIF).unwrap().read_to_end(&mut mifvec).unwrap();
//    let mut fmvec = vec![];
//    File::open(FM).unwrap().read_to_end(&mut fmvec).unwrap();
//
//    let mifstr = str::from_utf8(&mifvec).unwrap();
//
//    let mut uniqs = HashMap::<u32, Option<usize>>::new();
//
//    let re = Regex::new(r"<Unique (\d+)>").unwrap();
//    for cap in re.captures_iter(mifstr) {
//        let uniq = cap.at(1).unwrap();
//        let uniq: u32 = uniq.parse().unwrap();
//        //println!("{}", uniq);
//        let prev = uniqs.insert(uniq, None);
//        // 0 is the BookComponent, which may be duplicated apparently
//        assert!(prev.is_none() || uniq == 0);
//    }
//    let prev = uniqs.remove(&0);
//    assert!(prev.is_some());
//
//    println!("found {} uniqs", uniqs.len());
//
//    for (uniq, position) in uniqs.iter_mut() {
//        let uniq_be = uniq.to_be();
//        let uniq_be: [u8; 4] = unsafe { mem::transmute(uniq_be) };
//        for (pos, fmwindow) in fmvec.windows(mem::size_of::<u32>()).enumerate() {
//            if &uniq_be == fmwindow {
//                assert!(position.is_none());
//                *position = Some(pos)
//            }
//        }
//        assert!(position.is_some());
//    }
//
//    let uniqs: HashMap<u32, usize> = uniqs.into_iter()
//        .map(|(k, v)| (k, v.unwrap())).collect();
//    let mut ordered_uniqs: Vec<(usize, u32)> = uniqs.iter()
//        .map(|(k, v)| (*v, *k)).collect();
//    ordered_uniqs.sort_by(|&(ref pos1, _), &(ref pos2, _)| pos1.cmp(pos2));
//    let gaps: Vec<Option<usize>> = ordered_uniqs.windows(2)
//        .map(|window| Some(window[1].0 - window[0].0 - mem::size_of::<u32>()))
//        .chain(iter::once(None)).collect();
//    assert!(ordered_uniqs.len() == gaps.len());
//
//    {
//    let mut uniqposfile = File::create("uniqpos").unwrap();
//        for (&(pos, uniq), &gap) in ordered_uniqs.iter().zip(gaps.iter()) {
//            let suffixpos = pos + mem::size_of::<u32>();
//            let suffixbytes = [fmvec[suffixpos], fmvec[suffixpos+1]];
//            if let Some(gap) = gap {
//                writeln!(uniqposfile, "{:08x} => {} ({:?} byte => {} gap)", pos, uniq, suffixbytes, gap).unwrap();
//            } else {
//                writeln!(uniqposfile, "{:08x} => {} ({:?} byte)", pos, uniq, suffixbytes).unwrap();
//            }
//        }
//    }
//
//    let firstuniqpos = ordered_uniqs.first().unwrap().0;
//    let lastuniqpos = ordered_uniqs.last().unwrap().0;
//    println!("firstuniq: {:08x}", firstuniqpos);
//    println!("lastuniq:  {:08x}", lastuniqpos);
//
//    let mut pos = firstuniqpos;
//    loop {
//        let v = &fmvec[pos..];
//        let uniq: [u8; 4] = [v[0], v[1], v[2], v[3]];
//        let uniq = unsafe { mem::transmute(uniq) };
//        let uniq = u32::from_be(uniq);
//        let isuniq = uniqs.contains_key(&uniq);
//        if !isuniq {
//            println!("(0x{:08x}) strange uniq {}", pos, uniq);
//        } else {
//            println!("(0x{:08x}) good uniq {}", pos, uniq);
//        }
//        let v = &v[4..];
//        pos += 4;
//        match v[0] {
//            2 | 8 => {
//                assert!(isuniq);
//                pos += 2
//            },
//            1 => {
//                assert!(!isuniq);
//                if v[1] == 0x80 {
//                    pos += 3
//                } else {
//                    pos += 2
//                }
//            },
//            3 | 4 => {
//                assert!(!isuniq);
//                pos += 2
//            },
//            b => {
//                println!("odd post-uniq byte: {}", b);
//                panic!()
//            },
//        }
//    }
//}
