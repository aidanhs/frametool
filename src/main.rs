#![recursion_limit="200"]
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
    use nom::{is_alphanumeric, is_digit};
    use nom::multispace;
    use nom::non_empty;
    use nom::not_line_ending;
    use nom::whitespace::sp;

    #[derive(Debug)]
    enum MIFTree<'a> {
        MIFFile(&'a str),
        // MIFNOTE: undocumented
        Units(&'a [u8]),
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

        PgfCatalog(Vec<MIFTree<'a>>),
        Pgf(Vec<MIFTree<'a>>),
        PgfTag(&'a str),
        PgfUseNextTag(bool),
        PgfNextTag(&'a str),
        PgfFIndent((f64, MIFUnit)),
        PgfFIndentRelative(bool),
        PgfFIndentOffset((f64, MIFUnit)),
        PgfLIndent((f64, MIFUnit)),
        PgfRIndent((f64, MIFUnit)),
        PgfAlignment(MIFKeyword<'a>),
        PgfDir(MIFKeyword<'a>),
        PgfSpBefore((f64, MIFUnit)),
        PgfSpAfter((f64, MIFUnit)),
        PgfLineSpacing(MIFKeyword<'a>),
        PgfLeading((f64, MIFUnit)),
        PgfNumTabs(u64),
        TabStop(Vec<MIFTree<'a>>),
        TSX((f64, MIFUnit)),
        TSType(MIFKeyword<'a>),
        TSLeaderStr(&'a str),
        TSDecimalChar(u64),
        PgfPlacement(MIFKeyword<'a>),
        PgfPlacementStyle(MIFKeyword<'a>),
        PgfRunInDefaultPunct(&'a str),
        PgfWithPrev(bool),
        PgfWithNext(bool),
        PgfBlockSize(u64),
        PgfAutoNum(bool),
        PgfNumFormat(&'a str),
        PgfNumberFont(&'a str),
        PgfNumAtEnd(bool),
        PgfHyphenate(bool),
        HyphenMaxLines(u64),
        HyphenMinPrefix(u64),
        HyphenMinSuffix(u64),
        HyphenMinWord(u64),
        PgfLetterSpace(bool),
        PgfMinWordSpace(u64),
        PgfOptWordSpace(u64),
        PgfMaxWordSpace(u64),
        PgfLanguage(MIFKeyword<'a>),
        PgfTopSeparator(&'a str),
        PgfTopSepAtIndent(bool),
        PgfTopSepOffset((f64, MIFUnit)),
        PgfBoxColor(MIFKeyword<'a>),
        PgfBotSeparator(&'a str),
        PgfBotSepAtIndent(bool),
        PgfBotSepOffset((f64, MIFUnit)),
        PgfCellAlignment(MIFKeyword<'a>),
        PgfCellMargins(((f64, MIFUnit), (f64, MIFUnit), (f64, MIFUnit), (f64, MIFUnit))),
        PgfCellLMarginFixed(bool),
        PgfCellTMarginFixed(bool),
        PgfCellRMarginFixed(bool),
        PgfCellBMarginFixed(bool),
        PgfLocked(bool),
        PgfAcrobatLevel(u64),
        // MIFNOTE: these 7 aren't really documented
        PgfMinJRomanLetterSpace(f64),
        PgfOptJRomanLetterSpace(f64),
        PgfMaxJRomanLetterSpace(f64),
        PgfMinJLetterSpace(f64),
        PgfOptJLetterSpace(f64),
        PgfMaxJLetterSpace(f64),
        PgfYakumonoType(MIFKeyword<'a>),
        // MIFNOTE: undocumented
        PgfPDFStructureLevel(u64),

        PgfFont(Vec<MIFTree<'a>>),
        Font(Vec<MIFTree<'a>>),
        FTag(&'a str),
        FFamily(&'a str),
        FAngle(&'a str),
        FWeight(&'a str),
        FVar(&'a str),
        FPostScriptName(&'a str),
        FPlatformName(&'a str),
        FLanguage(MIFKeyword<'a>),
        FEncoding(&'a str),
        FSize((f64, MIFUnit)),
        FColor(&'a str),
        FSeparation(u64),
        FStretch(f64),
        FBackgroundColor(MIFKeyword<'a>),
        FUnderlining(MIFKeyword<'a>),
        FOverline(bool),
        FStrike(bool),
        FChangeBar(bool),
        FPosition(MIFKeyword<'a>),
        FOutline(bool),
        FShadow(bool),
        FPairKern(bool),
        FCase(MIFKeyword<'a>),
        FDX(f64),
        FDY(f64),
        FDW(f64),
        FTsume(bool),
        FPlain(bool),
        FBold(bool),
        FItalic(bool),
        FLocked(bool),
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

    // TODO: placeholder so I don't have to specify all possible values
    // everywhere up-front - eventually this should be removed
    #[derive(Debug)]
    struct MIFKeyword<'a>(&'a [u8]);

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
                preceded!(opt!(char!(b'-')), alt_complete!(
                    delimited!(digit, tag!("."), digit)
                  | digit
                ))
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
    // MIFNOTE: since this is undocumented, dunno the possible values
    named!(mif_parse_Uunit<&[u8]>,
        do_parse!(tag!(b"U") >> unit: alphanumeric >> (unit))
    );
    named!(mif_parse_unit<MIFUnit>,
        alt_complete!(
            tag!(b" cm") =>     { |_| MIFUnit::Centimeters }
          | tag!(b" cicero") => { |_| MIFUnit::Ciceros }
          | tag!(b" cc") =>     { |_| MIFUnit::Ciceros }
          | tag!(b" dd") =>     { |_| MIFUnit::Didots }
          | tag!(b" in") =>     { |_| MIFUnit::Inches }
          | tag!(b"\"") =>      { |_| MIFUnit::Inches }
          | tag!(b" mm") =>     { |_| MIFUnit::Millimeters }
          | tag!(b" pica") =>   { |_| MIFUnit::Picas }
          | tag!(b" pi") =>     { |_| MIFUnit::Picas }
          | tag!(b" pc") =>     { |_| MIFUnit::Picas }
          | tag!(b" point") =>  { |_| MIFUnit::Points }
          | tag!(b" pt") =>     { |_| MIFUnit::Points }
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

    // TODO: should eventually disappear, see note on MIFKeyword
    fn is_keyword_char(b: u8) -> bool {
        is_alphanumeric(b) || b == b'.' || b == b'-'
    }
    named!(mif_parse_keyword<MIFKeyword>,
        do_parse!(x: take_while1!(is_keyword_char) >> (MIFKeyword(x)))
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
    named!(data_dimension<(f64, MIFUnit)>,
        pair!(mif_parse_decimal, mif_parse_unit)
    );
    named!(data_degrees<f64>, call!(mif_parse_decimal));
    // MIFNOTE: documented as having no units, but actually occasionally has %
    named!(data_percentage<f64>, terminated!(mif_parse_decimal, opt!(char!(b'%'))));
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
    named!(data_L_T_R_B<((f64, MIFUnit), (f64, MIFUnit), (f64, MIFUnit), (f64, MIFUnit))>,
        do_parse!(
            l: pair!(mif_parse_decimal, mif_parse_unit) >>
            spaces_and_comments >>
            t: pair!(mif_parse_decimal, mif_parse_unit) >>
            spaces_and_comments >>
            r: pair!(mif_parse_decimal, mif_parse_unit) >>
            spaces_and_comments >>
            b: pair!(mif_parse_decimal, mif_parse_unit) >>
            (l, t, r, b)
        )
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

    st!(PgfCatalog, (pgfs: many0!(stparse!(Pgf))), (pgfs));
    // TODO: pgfnumtabs must come before tabstop statements
    st!(Pgf, (props: many0!(alt_complete!(
            stparse!(PgfTag)
          | stparse!(PgfUseNextTag)
          | stparse!(PgfNextTag)
          | stparse!(PgfFIndent)
          | stparse!(PgfFIndentRelative)
          | stparse!(PgfFIndentOffset)
          | stparse!(PgfLIndent)
          | stparse!(PgfRIndent)
          | stparse!(PgfAlignment)
          | stparse!(PgfDir)
          | stparse!(PgfSpBefore)
          | stparse!(PgfSpAfter)
          | stparse!(PgfLineSpacing)
          | stparse!(PgfLeading)
          | stparse!(PgfNumTabs)
          | stparse!(TabStop)
          | stparse!(PgfFont)
          | stparse!(PgfPlacement)
          | stparse!(PgfPlacementStyle)
          | stparse!(PgfRunInDefaultPunct)
          | stparse!(PgfWithPrev)
          | stparse!(PgfWithNext)
          | stparse!(PgfBlockSize)
          | stparse!(PgfAutoNum)
          | stparse!(PgfNumFormat)
          | stparse!(PgfNumberFont)
          | stparse!(PgfNumAtEnd)
          | stparse!(PgfHyphenate)
          | stparse!(HyphenMaxLines)
          | stparse!(HyphenMinPrefix)
          | stparse!(HyphenMinSuffix)
          | stparse!(HyphenMinWord)
          | stparse!(PgfLetterSpace)
          | stparse!(PgfMinWordSpace)
          | stparse!(PgfOptWordSpace)
          | stparse!(PgfMaxWordSpace)
          | stparse!(PgfLanguage)
          | stparse!(PgfTopSeparator)
          | stparse!(PgfTopSepAtIndent)
          | stparse!(PgfTopSepOffset)
          | stparse!(PgfBoxColor)
          | stparse!(PgfBotSeparator)
          | stparse!(PgfBotSepAtIndent)
          | stparse!(PgfBotSepOffset)
          | stparse!(PgfCellAlignment)
          | stparse!(PgfCellMargins)
          | stparse!(PgfCellLMarginFixed)
          | stparse!(PgfCellTMarginFixed)
          | stparse!(PgfCellRMarginFixed)
          | stparse!(PgfCellBMarginFixed)
          | stparse!(PgfLocked)
          | stparse!(PgfAcrobatLevel)
          | stparse!(PgfMinJRomanLetterSpace)
          | stparse!(PgfOptJRomanLetterSpace)
          | stparse!(PgfMaxJRomanLetterSpace)
          | stparse!(PgfMinJLetterSpace)
          | stparse!(PgfOptJLetterSpace)
          | stparse!(PgfMaxJLetterSpace)
          | stparse!(PgfYakumonoType)
          | stparse!(PgfPDFStructureLevel)
        ))),
        (props)
    );
    st!(PgfTag, (val: data_tagstring), (val));
    st!(PgfUseNextTag, (val: data_boolean), (val));
    st!(PgfNextTag, (val: data_tagstring), (val));
    st!(PgfFIndent, (val: data_dimension), (val));
    st!(PgfFIndentRelative, (val: data_boolean), (val));
    st!(PgfFIndentOffset, (val: data_dimension), (val));
    st!(PgfLIndent, (val: data_dimension), (val));
    st!(PgfRIndent, (val: data_dimension), (val));
    st!(PgfAlignment, (val: mif_parse_keyword), (val));
    st!(PgfDir, (val: mif_parse_keyword), (val));
    st!(PgfSpBefore, (val: data_dimension), (val));
    st!(PgfSpAfter, (val: data_dimension), (val));
    st!(PgfLineSpacing, (val: mif_parse_keyword), (val));
    st!(PgfLeading, (val: data_dimension), (val));
    st!(PgfNumTabs, (val: data_integer), (val));
    st!(TabStop,
        (props: many0!(alt_complete!(
            stparse!(TSX)
          | stparse!(TSType)
          | stparse!(TSLeaderStr)
          | stparse!(TSDecimalChar)
        ))), (props)
    );
    st!(TSX, (val: data_dimension), (val));
    st!(TSType, (val: mif_parse_keyword), (val));
    st!(TSLeaderStr, (val: data_string), (val));
    st!(TSDecimalChar, (val: data_integer), (val));
    st!(PgfPlacement, (val: mif_parse_keyword), (val));
    st!(PgfPlacementStyle, (val: mif_parse_keyword), (val));
    st!(PgfRunInDefaultPunct, (val: data_string), (val));
    st!(PgfWithPrev, (val: data_boolean), (val));
    st!(PgfWithNext, (val: data_boolean), (val));
    st!(PgfBlockSize, (val: data_integer), (val));
    st!(PgfAutoNum, (val: data_boolean), (val));
    st!(PgfNumFormat, (val: data_string), (val));
    st!(PgfNumberFont, (val: data_tagstring), (val));
    st!(PgfNumAtEnd, (val: data_boolean), (val));
    st!(PgfHyphenate, (val: data_boolean), (val));
    st!(HyphenMaxLines, (val: data_integer), (val));
    st!(HyphenMinPrefix, (val: data_integer), (val));
    st!(HyphenMinSuffix, (val: data_integer), (val));
    st!(HyphenMinWord, (val: data_integer), (val));
    st!(PgfLetterSpace, (val: data_boolean), (val));
    st!(PgfMinWordSpace, (val: data_integer), (val));
    st!(PgfOptWordSpace, (val: data_integer), (val));
    st!(PgfMaxWordSpace, (val: data_integer), (val));
    st!(PgfLanguage, (val: mif_parse_keyword), (val));
    st!(PgfTopSeparator, (val: data_string), (val));
    st!(PgfTopSepAtIndent, (val: data_boolean), (val));
    st!(PgfTopSepOffset, (val: data_dimension), (val));
    // MIFNOTE: documented as just a tagstring, not sure what NoColor is
    // but I don't currently have any other examples
    st!(PgfBoxColor, (val: tag!(b"NoColor")), (MIFKeyword(val)));
    st!(PgfBotSeparator, (val: data_string), (val));
    st!(PgfBotSepAtIndent, (val: data_boolean), (val));
    st!(PgfBotSepOffset, (val: data_dimension), (val));
    st!(PgfCellAlignment, (val: mif_parse_keyword), (val));
    st!(PgfCellMargins, (val: data_L_T_R_B), (val));
    st!(PgfCellLMarginFixed, (val: data_boolean), (val));
    st!(PgfCellTMarginFixed, (val: data_boolean), (val));
    st!(PgfCellRMarginFixed, (val: data_boolean), (val));
    st!(PgfCellBMarginFixed, (val: data_boolean), (val));
    st!(PgfLocked, (val: data_boolean), (val));
    st!(PgfAcrobatLevel, (val: data_integer), (val));
    st!(PgfMinJRomanLetterSpace, (val: data_percentage), (val));
    st!(PgfOptJRomanLetterSpace, (val: data_percentage), (val));
    st!(PgfMaxJRomanLetterSpace, (val: data_percentage), (val));
    st!(PgfMinJLetterSpace, (val: data_percentage), (val));
    st!(PgfOptJLetterSpace, (val: data_percentage), (val));
    st!(PgfMaxJLetterSpace, (val: data_percentage), (val));
    // MIFNOTE: 'documented' (not really documented) as a string, but not
    // sure what Floating is and don't have any other examples
    st!(PgfYakumonoType, (val: tag!(b"Floating")), (MIFKeyword(val)));
    st!(PgfPDFStructureLevel, (val: data_integer), (val));

    st!(PgfFont, (props: many0!(fontparts)), (props));
    st!(Font, (props: many0!(fontparts)), (props));
    named!(fontparts<MIFTree>,
        alt_complete!(
            stparse!(FTag)
          | stparse!(FFamily)
          | stparse!(FAngle)
          | stparse!(FWeight)
          | stparse!(FVar)
          | stparse!(FPostScriptName)
          | stparse!(FPlatformName)
          | stparse!(FLanguage)
          | stparse!(FEncoding)
          | stparse!(FSize)
          | stparse!(FColor)
          | stparse!(FSeparation)
          | stparse!(FStretch)
          | stparse!(FBackgroundColor)
          | stparse!(FUnderlining)
          | stparse!(FOverline)
          | stparse!(FStrike)
          | stparse!(FChangeBar)
          | stparse!(FPosition)
          | stparse!(FOutline)
          | stparse!(FShadow)
          | stparse!(FPairKern)
          | stparse!(FCase)
          | stparse!(FDX)
          | stparse!(FDY)
          | stparse!(FDW)
          | stparse!(FTsume)
          | stparse!(FPlain)
          | stparse!(FBold)
          | stparse!(FItalic)
          | stparse!(FLocked)
        )
    );

    st!(FTag, (val: data_tagstring), (val));
    st!(FFamily, (val: data_string), (val));
    st!(FAngle, (val: data_string), (val));
    st!(FWeight, (val: data_string), (val));
    st!(FVar, (val: data_string), (val));
    st!(FPostScriptName, (val: data_string), (val));
    st!(FPlatformName, (val: data_string), (val));
    st!(FLanguage, (val: mif_parse_keyword), (val));
    // MIFNOTE: documented to be a keyword, appears to actually be a string
    st!(FEncoding, (val: data_string), (val));
    st!(FSize, (val: data_dimension), (val));
    st!(FColor, (val: data_tagstring), (val));
    st!(FSeparation, (val: data_integer), (val));
    st!(FStretch, (val: data_percentage), (val));
    // MIFNOTE: documented as just a tagstring, not sure what NoColor is
    // but I don't currently have any other examples
    st!(FBackgroundColor, (val: tag!(b"NoColor")), (MIFKeyword(val)));
    st!(FUnderlining, (val: mif_parse_keyword), (val));
    st!(FOverline, (val: data_boolean), (val));
    st!(FStrike, (val: data_boolean), (val));
    st!(FChangeBar, (val: data_boolean), (val));
    st!(FPosition, (val: mif_parse_keyword), (val));
    st!(FOutline, (val: data_boolean), (val));
    st!(FShadow, (val: data_boolean), (val));
    st!(FPairKern, (val: data_boolean), (val));
    st!(FCase, (val: mif_parse_keyword), (val));
    st!(FDX, (val: data_percentage), (val));
    st!(FDY, (val: data_percentage), (val));
    st!(FDW, (val: data_percentage), (val));
    st!(FTsume, (val: data_boolean), (val));
    st!(FPlain, (val: data_boolean), (val));
    st!(FBold, (val: data_boolean), (val));
    st!(FItalic, (val: data_boolean), (val));
    st!(FLocked, (val: data_boolean), (val));

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
          | stparse!(PgfCatalog)
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
