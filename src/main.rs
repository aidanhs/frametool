#![recursion_limit="200"]
#![feature(plugin,range_contains,trace_macros)]
#![plugin(interpolate_idents)]
#![feature(field_init_shorthand)]

extern crate combine;
#[macro_use]
extern crate nom;

use std::borrow::Cow;
use std::cmp;
use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::str;

use parser::MIFTree;
use parser::MIFChar;

fn main() {
    mif2asciidoc()
}

#[derive(Debug)]
struct TextFlow<'a> {
    paras: Vec<Para<'a>>,
}
#[derive(Debug)]
struct Para<'a> {
    lines: Vec<Vec<LinePart<'a>>>,
}
#[derive(Debug)]
enum LinePart<'a> {
    Str(u64, &'a str), // trid, str
    Char(u64, MIFChar), // trid, char
}

fn mif2asciidoc() {
    let miffile = env::args().skip(1).next().expect("Expected one arg: file.mif");

    let mut mifvec = vec![];
    File::open(miffile).unwrap().read_to_end(&mut mifvec).unwrap();
    mifvec.retain(|&b| b != b'\r');
    mifvec.push(b'\n');

    //let mifstr = str::from_utf8(&mifvec).unwrap();
    let miftokens = lex::lex(&mifvec);
    let tree = parser::parse(&miftokens);

    //println!("DONE: {:?}\n\n\nunparsed:{:?}", miftree, unparsed);

    fn gettextflow<'a, 'b>(e: &'b MIFTree<'a>) -> Option<&'b [MIFTree<'a>]> {
        if let MIFTree::TextFlow(ref es) = *e { Some(es) } else { None }
    }
    fn getpara<'a, 'b>(e: &'b MIFTree<'a>) -> Option<&'b [MIFTree<'a>]> {
        if let MIFTree::Para(ref es) = *e { Some(es) } else { None }
    }
    fn getparaline<'a, 'b>(e: &'b MIFTree<'a>) -> Option<&'b [MIFTree<'a>]> {
        if let MIFTree::ParaLine(ref es) = *e { Some(es) } else { None }
    }
    fn gettextrectid<'a, 'b>(e: &'b MIFTree<'a>) -> Option<u64> {
        if let MIFTree::TextRectID(id) = *e { Some(id) } else { None }
    }
    let mut textflows = vec![];
    for elts in tree.iter().filter_map(gettextflow) {
        let mut curtextrectid = None;
        let mut paras = vec![];
        for elts in elts.iter().filter_map(getpara) {
            let mut lines = vec![];
            for elts in elts.iter().filter_map(getparaline) {
                let trids: Vec<_> = elts.iter()
                    .filter_map(gettextrectid).collect();
                match trids.len() {
                    0 => (),
                    1 => {
                        // Dunno what to do with a textrectid if it's not
                        // at the beginning of a paraline
                        assert!(gettextrectid(&elts[0]).is_some());
                        curtextrectid = Some(trids[0])
                    }
                    v => panic!("trids {:?} with {:?}", trids, v),
                }
                let mut lineparts = vec![];
                let trid = curtextrectid.unwrap();
                for elt in elts.iter() {
                    lineparts.push(match *elt {
                        MIFTree::String(s) => LinePart::Str(trid, s),
                        MIFTree::Char(c) => LinePart::Char(trid, c),
                        _ => continue,
                    })
                }
                if lineparts.len() > 0 {
                    lines.push(lineparts)
                }
            }
            // might just have a var or tag etc
            if lines.len() > 0 {
                let para = Para { lines };
                paras.push(para)
            }
        }
        if paras.len() > 0 {
            let textflow = TextFlow { paras };
            textflows.push(textflow)
        }
    }
    // Textflows aren't guaranteed to be in order, we should really use TextRect
    // objects (inside Page/Frame as an object), but it's close enough for our
    // purposes
    for textflow in textflows.iter() {
        for para in textflow.paras.iter() {
            for paraline in para.lines.iter() {
                let mut paralinestr = String::new();
                for paralinepart in paraline {
                    match *paralinepart {
                        LinePart::Str(_, s) => paralinestr.push_str(s),
                        LinePart::Char(_, c) => paralinestr.push(c.as_char()),
                    }
                }
                println!("{}", paralinestr);
            }
        }
        println!("\n=======================\n");
    }
}

fn briefer_slice<T>(sl: &[T]) -> &[T] {
    &sl[..cmp::min(sl.len(), 100)]
}
fn briefer_utf8(bs: &[u8]) -> Cow<str> {
    String::from_utf8_lossy(briefer_slice(bs))
}

mod lex;

mod parser {
    #![allow(non_snake_case)]

    use super::briefer_slice;
    use super::lex::MIFToken;

    use std::ascii::AsciiExt;
    use std::ops::Range;
    use std::str;
    use std::str::FromStr;

    use nom;
    use nom::Err;
    use nom::ErrorKind;
    use nom::IResult;
    use nom::alphanumeric;
    use nom::digit;

    type MIFDimension = (f64, MIFUnit);

    #[derive(Debug)]
    pub enum MIFTree<'a> {
        _Unknown(&'a str),
        _InsetData(&'a str),

        // across a few different elts
        Unique(u64),
        String(&'a str),
        Char(MIFChar),

        MIFFile(&'a str),
        // MIFNOTE: undocumented
        Units(&'a str),
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
        // MIFNOTE: undocumented, possibly a framemaker 8 remnant
        CAndCondition,

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
        PgfFIndent(MIFDimension),
        PgfFIndentRelative(bool),
        PgfFIndentOffset(MIFDimension),
        PgfLIndent(MIFDimension),
        PgfRIndent(MIFDimension),
        PgfAlignment(MIFKeyword<'a>),
        PgfDir(MIFKeyword<'a>),
        PgfSpBefore(MIFDimension),
        PgfSpAfter(MIFDimension),
        PgfLineSpacing(MIFKeyword<'a>),
        PgfLeading(MIFDimension),
        PgfNumTabs(u64),
        TabStop(Vec<MIFTree<'a>>),
        TSX(MIFDimension),
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
        PgfTopSepOffset(MIFDimension),
        PgfBoxColor(MIFKeyword<'a>),
        PgfBotSeparator(&'a str),
        PgfBotSepAtIndent(bool),
        PgfBotSepOffset(MIFDimension),
        PgfCellAlignment(MIFKeyword<'a>),
        PgfCellMargins((MIFDimension, MIFDimension, MIFDimension, MIFDimension)),
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
        FSize(MIFDimension),
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

        TextFlow(Vec<MIFTree<'a>>),
        TFTag(&'a str),
        TFAutoConnect(bool),
        FlowDir,
        TFPostScript(bool),
        TFFeather(bool),
        TFSynchronized(bool),
        TFLineSpacing(MIFDimension),
        TFMinHangHeight(MIFDimension),
        TFSideHeads(bool),
        TFMaxInterLine(MIFDimension),
        TFMaxInterPgf(MIFDimension),
        Notes,
        Para(Vec<MIFTree<'a>>),
        PgfNumString(&'a str),
        PgfEndCond(bool),
        PgfCondFullPgf(bool),
        // MIFNOTE: undocumented
        PgfReferenced(bool),
        ParaLine(Vec<MIFTree<'a>>),
        ElementBegin,
        TextRectID(u64),
        InlineComponent,
        InlineComponentEnd,
        SpclHyphenation(bool),
        Conditional,
        Unconditional,
        ATbl(u64),
        AFrame(u64),
        FNote(u64),
        Marker(Vec<MIFTree<'a>>),
        MType(u64),
        MTypeName(&'a str),
        MText(&'a str),
        MCurrPage(u64),
        Variable(Vec<MIFTree<'a>>),
        VariableName(&'a str),
        VariableLocked(bool),
        XRef,
        ElementEnd,

        Page(Vec<MIFTree<'a>>),
        PageType(MIFKeyword<'a>),
        PageNum(&'a str),
        PageTag(&'a str),
        PageSize((MIFDimension, MIFDimension)),
        PageAngle(f64),
        // MIFNOTE: exists for backwards compatibility
        PageOrientation(MIFKeyword<'a>),
        PageBackground(&'a str),

        // generic object stuff
        ID(u64),
        GroupID(u64),
        Pen(u64),
        Fill(u64),
        PenWidth(MIFDimension),
        ObColor(&'a str),
        ObTint(f64),
        Separation(u64),
        Overprint(bool),
        RunaroundType(MIFKeyword<'a>),
        RunaroundGap(MIFDimension),
        Angle(f64),
        ReRotateAngle(MIFDimension),
        DashedPattern(Vec<MIFTree<'a>>),
        DashedStyle(MIFKeyword<'a>),
        NumSegments(u64),
        DashSegment(MIFDimension),
        ObjectAttribute(Vec<MIFTree<'a>>),
        Tag(&'a str),
        Value(&'a str),
        // commonish
        Smoothed(bool),
        ShapeRect((MIFDimension, MIFDimension, MIFDimension, MIFDimension)),
        // MIFNOTE: exists for backwards compatibility
        BRect((MIFDimension, MIFDimension, MIFDimension, MIFDimension)),

        // objects
        Frame(Vec<MIFTree<'a>>), //
        IsHotspot(bool),
        HotspotCmdStr(&'a str),
        HotspotTitle(&'a str),
        FrameType(MIFKeyword<'a>),
        AnchorDirection(MIFKeyword<'a>),
        Float(bool),
        NSOffset(MIFDimension),
        BLOffset(MIFDimension),
        AnchorAlign(MIFKeyword<'a>),
        AnchorBeside(MIFKeyword<'a>),
        Cropped(bool),
        ImportObject(Vec<MIFTree<'a>>), //
        ImportObFile(&'a str),
        ImportObFileDI(&'a str),
        ImportHint(&'a str),
        PosterFileDI(&'a str),
        BitMapDpi(u64),
        ImportObFixedSize(bool),
        FlipLR(bool),
        ImportObNameDI(&'a str),
        ObjectActivateInPDF(bool),
        ObjectOpenInFloatWindow(bool),
        ObjectSupportMMLink(bool),
        NativeOrigin((MIFDimension, MIFDimension)),
        ImportObEditor(&'a str),
        ImportObUpdater(&'a str),
        ImportURL(&'a str),
        ObjectInfo(&'a str),

        PolyLine(Vec<MIFTree<'a>>), //
        HeadCap(MIFKeyword<'a>),
        TailCap(MIFKeyword<'a>),
        ArrowStyle(Vec<MIFTree<'a>>),
        TipAngle(u64),
        BaseAngle(u64),
        Length(MIFDimension),
        HeadType(MIFKeyword<'a>),
        ScaleHead(bool),
        ScaleFactor(MIFDimension),
        NumPoints(u64),
        Point((MIFDimension, MIFDimension)),
        Rectangle(Vec<MIFTree<'a>>), //
        TextLine(Vec<MIFTree<'a>>), //
        TLOrigin((MIFDimension, MIFDimension)),
        TLAlignment(MIFKeyword<'a>),
        TLDirection(MIFKeyword<'a>),
        TLLanguage(MIFKeyword<'a>),
        TextRect(Vec<MIFTree<'a>>), //
        TRNext(u64),
        TRNumColumns(u64),
        TRColumnGap(MIFDimension),
        TRColumnBalance(bool),
        TRSideheadWidth(MIFDimension),
        TRSideheadGap(MIFDimension),
        TRSideheadPlacement(MIFKeyword<'a>),
    }

    enum MIFErr {
        OutOfRangeError,
    }

    #[derive(Copy, Clone, Debug)]
    pub enum MIFUnit {
        Centimeters,
        Ciceros,
        Didots,
        Inches,
        Millimeters,
        Picas,
        Points,
    }

    #[derive(Copy, Clone, Debug)]
    pub enum MIFChar {
        EmSpace,
        EnSpace,
        ThinSpace,
        HardSpace,
        SoftHyphen,
        DiscHyphen,
        Tab,
        HardReturn,
    }
    impl MIFChar {
        pub fn as_char(&self) -> char {
            use MIFChar::*;
            match *self {
                EmSpace => ' ',
                EnSpace => ' ',
                ThinSpace => ' ',
                HardSpace => ' ',
                SoftHyphen => '-',
                DiscHyphen => '-', // TODO: 'discretionary' hyphen, so remove
                Tab => '\t',
                HardReturn => '\n',
            }
        }
    }

    #[derive(Copy, Clone, Debug)]
    pub enum MIFCharUnit {
        Points,
        Q,
    }

    #[derive(Copy, Clone, Debug)]
    pub enum MIFColorAttributeKeyword {
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

    #[derive(Copy, Clone, Debug)]
    pub enum MIFCStateKeyword {
        CHidden,
        CShown,
    }

    #[derive(Copy, Clone, Debug)]
    pub enum MIFCStyleKeyword {
        CAsIs,
        CUnderline,
        CDoubleUnderline,
        CStrike,
        COverline,
        CChangeBar,
    }

    // TODO: probably incomplete
    #[derive(Copy, Clone, Debug)]
    pub enum MIFDiLanguage {
        USEnglish,
        UKEnglish,
    }

    // TODO: placeholder so I don't have to specify all possible values
    // everywhere up-front - eventually this should be removed
    #[derive(Copy, Clone, Debug)]
    pub struct MIFKeyword<'a>(pub &'a str);

    pub fn parse<'a>(tokens: &[MIFToken<'a>]) -> Vec<MIFTree<'a>> {
        match fullmiffile(tokens) {
            IResult::Done(unparsed, miftree) => {
                if unparsed.len() != 0 {
                    println!("UNPARSED:\n");
                    println!("{:?}", unparsed);
                    panic!("unparsed");
                }
                miftree
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
                            println!("POSITION: {:?} {:?}\n", e, briefer_slice(p));
                            break
                        },
                        Err::NodePosition(e, p, next) => {
                            println!("NODEPOSITION: {:?} {:?}\n", e, briefer_slice(p));
                            *next
                        },
                    }
                }
                panic!("error")
            },
            IResult::Incomplete(needed) => {
                println!("NEEDED: {:?}", needed);
                panic!("incomplete")
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

    // Ouch - without this, a trailing optional in a do_parse will make the
    // whole do_parse incomplete, which then errors
    // https://github.com/Geal/nom/issues/406
    macro_rules! optional (
        ($i:expr, $( $rest:tt )*) => (opt!($i, complete!($( $rest )*)));
    );

    macro_rules! tok (
        ($i:expr, $out:expr, $( $part:tt )*) => (
            {
                if $i.is_empty() {
                    let res: nom::IResult<_, _> = nom::IResult::Incomplete(nom::Needed::Size(1));
                    res
                } else {
                    #[allow(unused)]
                    use self::MIFToken::*;
                    match $i[0] {
                        $( $part )* => nom::IResult::Done(&$i[1..], $out),
                        _ => nom::IResult::Error(error_position!(nom::ErrorKind::Char, $i)),
                    }
                }
            }
        );
        ($i:expr, $( $part:tt )*) => (
            tok!($i, e, e @ $( $part )*)
        );
    );
    macro_rules! tokmap (
        ($i:expr, $( $rest:tt )*) => (
            flat_map!($i, tok!(u, Token(u)), do_parse!(x: $( $rest )* >> eof!() >> (x)))
        );
    );
    macro_rules! strmap (
        ($i:expr, $( $rest:tt )*) => (
            flat_map!($i, tok!(u, Str(u)), do_parse!(x: $( $rest )* >> eof!() >> (x)))
        );
    );

    macro_rules! mynamed (
        ($name:ident<$t:ty>, $submac:ident!( $( $args:tt )* )) => (
            fn $name<'a, 'b>(i: &'b [MIFToken<'a>]) -> IResult<&'b [MIFToken<'a>], $t, u32> {
                $submac!(i, $( $args )*)
            }
        )
    );

    named!(parse_decimal_str<&str, f64>,
        map_res!(recognize!(
            preceded!(optional!(tag!("-")), alt_complete!(
                delimited!(digit, tag!("."), digit)
              | digit
            ))
        ), f64::from_str)
    );

    mynamed!(mif_parse_decimal<f64>, tokmap!(parse_decimal_str));
    mynamed!(mif_parse_percentage<f64>,
        tokmap!(do_parse!(p: parse_decimal_str >> optional!(tag!("%")) >> (p)))
    );
    mynamed!(mif_parse_integer<u64>,
        tokmap!(map_res!(digit, |s| u64::from_str_radix(s, 10)))
    );
    mynamed!(mif_parse_string_integer<u64>,
        strmap!(map_res!(digit, |s| u64::from_str_radix(s, 10)))
    );
    mynamed!(mif_parse_char<MIFChar>,
        tokmap!(alt_complete!(
            tag!("EmSpace") => { |_| MIFChar::EmSpace }
          | tag!("EnSpace") => { |_| MIFChar::EnSpace }
          | tag!("ThinSpace") => { |_| MIFChar::ThinSpace }
          | tag!("HardSpace") => { |_| MIFChar::HardSpace }
          | tag!("SoftHyphen") => { |_| MIFChar::SoftHyphen }
          | tag!("DiscHyphen") => { |_| MIFChar::DiscHyphen }
          | tag!("Tab") => { |_| MIFChar::Tab }
          | tag!("HardReturn") => { |_| MIFChar::HardReturn }
          //| tag!("") => { |_| MIFChar:: }
        ))
    );
    mynamed!(mif_parse_charunit<MIFCharUnit>,
        tokmap!(do_parse!(tag!("CU") >> unit: alt_complete!(
            tag!("pt") => { |_| MIFCharUnit::Points }
          | tag!("Q") =>  { |_| MIFCharUnit::Q }
        ) >> (unit)))
    );
    // MIFNOTE: since this is undocumented, dunno the possible values
    mynamed!(mif_parse_Uunit<&'a str>,
        tokmap!(do_parse!(tag!("U") >> unit: alphanumeric >> (unit)))
    );
    // TODO: this doesn't currently handle " for inches as it's tokenised as one token
    // TODO: it's not clear whether it's valid to have more than one space between
    // number and unit - because tokenisation happens first, it is permitted
    mynamed!(mif_parse_num_unit<MIFDimension>,
        pair!(
            mif_parse_decimal,
            tokmap!(alt_complete!(
                tag!("cm") =>     { |_| MIFUnit::Centimeters }
              | tag!("cicero") => { |_| MIFUnit::Ciceros }
              | tag!("cc") =>     { |_| MIFUnit::Ciceros }
              | tag!("dd") =>     { |_| MIFUnit::Didots }
              | tag!("in") =>     { |_| MIFUnit::Inches }
              //| tag!("\"") =>      { |_| MIFUnit::Inches }
              | tag!("mm") =>     { |_| MIFUnit::Millimeters }
              | tag!("pica") =>   { |_| MIFUnit::Picas }
              | tag!("pi") =>     { |_| MIFUnit::Picas }
              | tag!("pc") =>     { |_| MIFUnit::Picas }
              | tag!("point") =>  { |_| MIFUnit::Points }
              | tag!("pt") =>     { |_| MIFUnit::Points }
            ))
        )
    );
    mynamed!(mif_parse_colorattributekeyword<MIFColorAttributeKeyword>,
        tokmap!(alt_complete!(
            tag!("ColorIsBlack") =>       { |_| MIFColorAttributeKeyword::ColorIsBlack }
          | tag!("ColorIsWhite") =>       { |_| MIFColorAttributeKeyword::ColorIsWhite }
          | tag!("ColorIsRed") =>         { |_| MIFColorAttributeKeyword::ColorIsRed }
          | tag!("ColorIsGreen") =>       { |_| MIFColorAttributeKeyword::ColorIsGreen }
          | tag!("ColorIsBlue") =>        { |_| MIFColorAttributeKeyword::ColorIsBlue }
          | tag!("ColorIsCyan") =>        { |_| MIFColorAttributeKeyword::ColorIsCyan }
          | tag!("ColorIsMagenta") =>     { |_| MIFColorAttributeKeyword::ColorIsMagenta }
          | tag!("ColorIsYellow") =>      { |_| MIFColorAttributeKeyword::ColorIsYellow }
          | tag!("ColorIsDarkGrey") =>    { |_| MIFColorAttributeKeyword::ColorIsDarkGrey }
          | tag!("ColorIsPaleGreen") =>   { |_| MIFColorAttributeKeyword::ColorIsPaleGreen }
          | tag!("ColorIsForestGreen") => { |_| MIFColorAttributeKeyword::ColorIsForestGreen }
          | tag!("ColorIsRoyalBlue") =>   { |_| MIFColorAttributeKeyword::ColorIsRoyalBlue }
          | tag!("ColorIsMauve") =>       { |_| MIFColorAttributeKeyword::ColorIsMauve }
          | tag!("ColorIsLightSalmon") => { |_| MIFColorAttributeKeyword::ColorIsLightSalmon }
          | tag!("ColorIsOlive") =>       { |_| MIFColorAttributeKeyword::ColorIsOlive }
          | tag!("ColorIsSalmon") =>      { |_| MIFColorAttributeKeyword::ColorIsSalmon }
          | tag!("ColorIsReserved") =>    { |_| MIFColorAttributeKeyword::ColorIsReserved }
          | tag!("ColorIsDarkYellow") =>  { |_| MIFColorAttributeKeyword::ColorIsDarkYellow }
        ))
    );
    mynamed!(mif_parse_cstatekeyword<MIFCStateKeyword>,
        tokmap!(alt_complete!(
            tag!("CHidden") => { |_| MIFCStateKeyword::CHidden }
          | tag!("CShown") =>  { |_| MIFCStateKeyword::CShown }
        ))
    );
    mynamed!(mif_parse_cstylekeyword<MIFCStyleKeyword>,
        tokmap!(alt_complete!(
            tag!("CAsIs") =>            { |_| MIFCStyleKeyword::CAsIs }
          | tag!("CUnderline") =>       { |_| MIFCStyleKeyword::CUnderline }
          | tag!("CDoubleUnderline") => { |_| MIFCStyleKeyword::CDoubleUnderline }
          | tag!("CStrike") =>          { |_| MIFCStyleKeyword::CStrike }
          | tag!("COverline") =>        { |_| MIFCStyleKeyword::COverline }
          | tag!("CChangeBar") =>       { |_| MIFCStyleKeyword::CChangeBar }
        ))
    );
    mynamed!(mif_parse_dilanguage<MIFDiLanguage>,
        tokmap!(alt_complete!(
            tag!("USEnglish") => { |_| MIFDiLanguage::USEnglish }
          | tag!("UKEnglish") => { |_| MIFDiLanguage::UKEnglish }
        ))
    );

    mynamed!(mif_parse_inset<MIFTree<'a>>,
        tok!(MIFTree::_InsetData(content), Inset { ty: _, content })
    );

    // TODO: should eventually disappear, see note on MIFKeyword
    fn is_keyword_char(c: char) -> bool {
        c.is_ascii() && (c.is_alphanumeric() || c == '.' || c == '-')
    }
    // TODO: does this just leave any trailing chars?
    mynamed!(mif_parse_keyword<MIFKeyword<'a>>,
        tokmap!(do_parse!(x: take_while1!(is_keyword_char) >> (MIFKeyword(x))))
    );

    mynamed!(data_string<&'a str>, tok!(s, Str(s)));
    mynamed!(data_tagstring<&'a str>, tok!(s, Str(s)));
    // TODO: don't just return the untransformed value
    mynamed!(data_path<&'a str>, tok!(s, Str(s)));
    mynamed!(data_boolean<bool>,
        tokmap!(alt_complete!(
            tag!("Yes") => { |_| true }
          | tag!("No") =>  { |_| false }
        ))
    );
    mynamed!(data_integer<u64>, call!(mif_parse_integer));
    mynamed!(data_string_integer<u64>, call!(mif_parse_string_integer));
    // MIFNOTE: "An ID can be any positive integer between 1 and 65535,
    // inclusive" - you liar
    mynamed!(data_ID<u64>,
        map_res!(mif_parse_integer, |i| checkrange(i, 1..99999999))
    );
    mynamed!(data_dimension<MIFDimension>, call!(mif_parse_num_unit));
    mynamed!(data_degrees<f64>, call!(mif_parse_decimal));
    //// MIFNOTE: documented as having no units, but actually occasionally has %
    mynamed!(data_percentage<f64>, call!(mif_parse_percentage));
    // TODO: don't just return the untransformed value
    mynamed!(data_metric<&'a str>,
        // TODO
        call!(fail)
    );
    mynamed!(data_W_H<(MIFDimension, MIFDimension)>,
        do_parse!(
            w: mif_parse_num_unit >> h: mif_parse_num_unit >>
            (w, h)
        )
    );
    mynamed!(data_X_Y<(MIFDimension, MIFDimension)>,
        do_parse!(
            x: mif_parse_num_unit >> y: mif_parse_num_unit >>
            (x, y)
        )
    );
    mynamed!(data_L_T_R_B<(MIFDimension, MIFDimension, MIFDimension, MIFDimension)>,
        do_parse!(
            l: mif_parse_num_unit >> t: mif_parse_num_unit >>
            r: mif_parse_num_unit >> b: mif_parse_num_unit >>
            (l, t, r, b)
        )
    );
    mynamed!(data_L_T_W_H<(MIFDimension, MIFDimension, MIFDimension, MIFDimension)>,
        do_parse!(
            l: mif_parse_num_unit >> t: mif_parse_num_unit >>
            w: mif_parse_num_unit >> h: mif_parse_num_unit >>
            (l, t, w, h)
        )
    );
    // TODO: don't just return the untransformed value
    mynamed!(data_X_Y_W_H<&'a str>,
        // TODO
        call!(fail)
    );

    fn unknown_miftree<'a, 'b>(input: &'b [MIFToken<'a>]) -> IResult<&'b [MIFToken<'a>], MIFTree<'a>, u32> {
        use nom::IResult::{Done, Error};
        use nom::{InputLength, Slice};
        use self::MIFToken::{StartStatement, EndStatement, Token};
        let input_length = input.input_len();
        if input_length < 3 {
            return Error(error_position!(ErrorKind::Custom(3378), input))
        }
        let tree = MIFTree::_Unknown(match (input[0], input[1]) {
            (StartStatement, Token(name)) => name,
            _ => return Error(error_position!(ErrorKind::Custom(4378), input))
        });
        let mut depth = 0;
        for (idx, item) in input.iter().enumerate() {
            match *item {
                StartStatement => depth += 1,
                EndStatement if depth > 1 => depth -= 1,
                EndStatement => return Done(input.slice(idx+1..), tree),
                _ => (),
            }
        }
        Error(error_position!(ErrorKind::Custom(5378), input))
    }
    macro_rules! __st_check (
        ($name:ident) => (
            interpolate_idents! {
                fn [check_ $name]<'a, 'b>(i: &'b [MIFToken<'a>]) -> IResult<&'b [MIFToken<'a>], (), u32> {
                    peek!(i,
                        do_parse!(
                            tok!(StartStatement) >>
                            tok!(Token(stringify!($name))) >>
                            (())
                        )
                    )
                }
            }
        )
    );
    macro_rules! __st_parse (
        ($name:ident, ($( $parsepart:tt )*)) => (
            interpolate_idents! {
                fn [statement_ $name]<'a, 'b>(i: &'b [MIFToken<'a>]) -> IResult<&'b [MIFToken<'a>], MIFTree<'a>, u32> {
                    delimited!(i,
                        do_parse!(
                            tok!(StartStatement) >>
                            tok!(Token(stringify!($name))) >>
                            (())
                        ),
                        do_parse!(
                            $( $parsepart )*
                        ),
                        tok!(MIFToken::EndStatement)
                    )
                }
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

    st!(Unique, (val: data_ID), (val));

    fn isfloatstr(s: &str) -> bool { s.bytes().all(|b| b == b'.' || (b >= b'0' && b <= b'9')) }
    st!(MIFFile, (vsn: tok!(f, Token(f) if isfloatstr(f))), (vsn));
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
          | stparse!(CAndCondition)
        ))),
        (props)
    );
    st!(CTag, (tag: data_string), (tag));
    st!(CState, (keyword: mif_parse_cstatekeyword), (keyword));
    st!(CStyle, (keyword: mif_parse_cstylekeyword), (keyword));
    st!(CColor, (tag: data_tagstring), (tag));
    st!(CSeparation, (sep: data_integer), (sep));
    st!(CBackgroundColor, (tag: data_tagstring), (tag));
    st!(CAndCondition, (), ());

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
    st!(PgfBoxColor, (val: tokmap!(tag!("NoColor"))), (MIFKeyword(val)));
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
    st!(PgfYakumonoType, (val: tokmap!(tag!("Floating"))), (MIFKeyword(val)));
    st!(PgfPDFStructureLevel, (val: data_integer), (val));

    st!(PgfFont, (props: many0!(fontparts)), (props));
    st!(Font, (props: many0!(fontparts)), (props));
    mynamed!(fontparts<MIFTree<'a>>,
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
    st!(FBackgroundColor, (val: tokmap!(tag!("NoColor"))), (MIFKeyword(val)));
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

    st!(TextFlow,
        (props: many0!(alt_complete!(
            stparse!(TFTag)
          | stparse!(TFAutoConnect)
          | stparse!(FlowDir)
          | stparse!(TFPostScript)
          | stparse!(TFFeather)
          | stparse!(TFSynchronized)
          | stparse!(TFLineSpacing)
          | stparse!(TFMinHangHeight)
          | stparse!(TFSideHeads)
          | stparse!(TFMaxInterLine)
          | stparse!(TFMaxInterPgf)
          | stparse!(Notes)
          | stparse!(Para)
        ))), (props)
    );
    st!(TFTag, (val: data_tagstring), (val));
    st!(TFAutoConnect, (val: data_boolean), (val));
    // TODO: validate one of an enum
    st!(FlowDir, (), ());
    st!(TFPostScript, (val: data_boolean), (val));
    st!(TFFeather, (val: data_boolean), (val));
    st!(TFSynchronized, (val: data_boolean), (val));
    st!(TFLineSpacing, (val: data_dimension), (val));
    st!(TFMinHangHeight, (val: data_dimension), (val));
    st!(TFSideHeads, (val: data_boolean), (val));
    st!(TFMaxInterLine, (val: data_dimension), (val));
    st!(TFMaxInterPgf, (val: data_dimension), (val));
    // TODO
    st!(Notes, (), ());
    st!(Para,
        (props: many0!(alt_complete!(
            stparse!(Unique)
          | stparse!(PgfTag)
          | stparse!(Pgf)
          | stparse!(PgfNumString)
          | stparse!(PgfEndCond)
          | stparse!(PgfCondFullPgf)
          | stparse!(PgfReferenced)
          | stparse!(ParaLine)
        ))), (props)
    );
    st!(PgfNumString, (val: data_string), (val));
    st!(PgfEndCond, (val: data_boolean), (val));
    st!(PgfCondFullPgf, (val: data_boolean), (val));
    st!(PgfReferenced, (val: data_boolean), (val));
    st!(ParaLine,
        (props: many0!(alt_complete!(
            stparse!(ElementBegin)
          | stparse!(TextRectID)
          | stparse!(InlineComponent)
          | stparse!(InlineComponentEnd)
          | stparse!(SpclHyphenation)
          | stparse!(Font)
          | stparse!(Conditional)
          | stparse!(Unconditional)
          | stparse!(String)
          | stparse!(Char)
          | stparse!(ATbl)
          | stparse!(AFrame)
          | stparse!(FNote)
          | stparse!(Marker)
          | stparse!(Variable)
          | stparse!(XRef)
          | stparse!(ElementEnd)
        ))), (props)
    );
    // TODO
    st!(ElementBegin, (), ());
    st!(TextRectID, (val: data_ID), (val));
    // TODO
    st!(InlineComponent, (), ());
    st!(InlineComponentEnd, (), ());
    st!(SpclHyphenation, (val: data_boolean), (val));
    // TODO
    st!(Conditional, (), ());
    st!(Unconditional, (), ());
    st!(String, (val: data_string), (val));
    // TODO: transform into actual char
    st!(Char, (val: mif_parse_char), (val));
    st!(ATbl, (val: data_ID), (val));
    st!(AFrame, (val: data_ID), (val));
    st!(FNote, (val: data_ID), (val));
    st!(Marker,
        (props: many0!(alt_complete!(
            stparse!(Unique)
          | stparse!(MType)
          | stparse!(MTypeName)
          | stparse!(MText)
          | stparse!(MCurrPage)
        ))), (props)
    );
    // TODO: convert to enum
    st!(MType, (val: data_integer), (val));
    st!(MTypeName, (val: data_string), (val));
    st!(MText, (val: data_string), (val));
    // MIFNOTE: documented as integer, actually in a str
    st!(MCurrPage, (val: data_string_integer), (val));
    st!(Variable,
        (props: many0!(alt_complete!(
            // MIFNOTE: not documented to appear here
            stparse!(Unique)
          | stparse!(VariableName)
          | stparse!(VariableLocked)
        ))), (props)
    );
    st!(VariableName, (val: data_string), (val));
    st!(VariableLocked, (val: data_boolean), (val));
    // TODO
    st!(XRef, (), ());
    st!(ElementEnd, (), ());

    // Common stuff across object statements
    macro_rules! generic_object_statements (
        ($i:expr,) => {
            alt_complete!($i,
                stparse!(ID)
              | stparse!(GroupID)
              | stparse!(Unique)
              | stparse!(Pen)
              | stparse!(Fill)
              | stparse!(PenWidth)
              | stparse!(ObColor)
              | stparse!(ObTint)
              | stparse!(Separation)
              | stparse!(Overprint)
              | stparse!(RunaroundType)
              | stparse!(RunaroundGap)
              | stparse!(Angle)
              | stparse!(ReRotateAngle)
              | stparse!(DashedPattern)
              | stparse!(ObjectAttribute)
            )
        };
    );
    st!(ID, (val: data_ID), (val));
    st!(GroupID, (val: data_ID), (val));
    st!(Pen, (val: data_integer), (val));
    st!(Fill, (val: data_integer), (val));
    st!(PenWidth, (val: data_dimension), (val));
    st!(ObColor, (val: data_tagstring), (val));
    st!(ObTint, (val: data_percentage), (val));
    st!(Separation, (val: data_integer), (val));
    st!(Overprint, (val: data_boolean), (val));
    // TODO: validate enum
    st!(RunaroundType, (val: mif_parse_keyword), (val));
    st!(RunaroundGap, (val: data_dimension), (val));
    st!(Angle, (val: data_degrees), (val));
    st!(ReRotateAngle, (val: data_dimension), (val));
    st!(DashedPattern,
        (props: many0!(alt_complete!(
            stparse!(DashedStyle)
          | stparse!(NumSegments)
          | stparse!(DashSegment)
        ))), (props)
    );
    st!(DashedStyle, (val: mif_parse_keyword), (val));
    st!(NumSegments, (val: data_integer), (val));
    st!(DashSegment, (val: data_dimension), (val));
    st!(ObjectAttribute,
        (props: many0!(alt_complete!(
            stparse!(Tag)
          | stparse!(Value)
        ))), (props)
    );
    st!(Tag, (val: data_string), (val));
    st!(Value, (val: data_string), (val));
    // MIFNOTE: although these seem to be shared across a few different
    // objects, they're not actually listed in the table of generic statements
    st!(ShapeRect, (val: data_L_T_W_H), (val));
    st!(BRect, (val: data_L_T_W_H), (val));
    st!(Smoothed, (val: data_boolean), (val));

    // The graphical objects
    macro_rules! all_objects (
        ($i:expr,) => {
            alt_complete!($i,
                stparse!(Frame)
              | stparse!(ImportObject)
              | stparse!(PolyLine)
              | stparse!(Rectangle)
              | stparse!(TextLine)
              | stparse!(TextRect)
            )
        };
    );
    st!(Frame,
        (props: many0!(alt_complete!(
            generic_object_statements!()
          | stparse!(IsHotspot)
          | stparse!(HotspotCmdStr)
          | stparse!(HotspotTitle)
          | stparse!(ShapeRect)
          | stparse!(BRect)
          | stparse!(FrameType)
          | stparse!(AnchorDirection)
          // TODO: this is actually a tagstring, but we're reusing 'tag' (a
          // string) from the generic object attributes because of conflicting
          // names
          | stparse!(Tag)
          | stparse!(Float)
          | stparse!(NSOffset)
          | stparse!(BLOffset)
          | stparse!(AnchorAlign)
          | stparse!(AnchorBeside)
          | stparse!(Cropped)
          | all_objects!()
        ))), (props)
    );
    st!(IsHotspot, (val: data_boolean), (val));
    st!(HotspotCmdStr, (val: data_string), (val));
    st!(HotspotTitle, (val: data_string), (val));
    st!(FrameType, (val: mif_parse_keyword), (val));
    st!(AnchorDirection, (val: mif_parse_keyword), (val));
    st!(Float, (val: data_boolean), (val));
    st!(NSOffset, (val: data_dimension), (val));
    st!(BLOffset, (val: data_dimension), (val));
    st!(AnchorAlign, (val: mif_parse_keyword), (val));
    st!(AnchorBeside, (val: mif_parse_keyword), (val));
    st!(Cropped, (val: data_boolean), (val));
    st!(ImportObject,
        (props: many0!(alt_complete!(
            generic_object_statements!()
          | call!(mif_parse_inset)
          | stparse!(ImportObFile)
          | stparse!(ImportObFileDI)
          | stparse!(ImportHint)
          | stparse!(PosterFileDI)
          | stparse!(ShapeRect)
          | stparse!(BRect)
          | stparse!(BitMapDpi)
          | stparse!(ImportObFixedSize)
          | stparse!(FlipLR)
          | stparse!(ImportObNameDI)
          | stparse!(ObjectActivateInPDF)
          | stparse!(ObjectOpenInFloatWindow)
          | stparse!(ObjectSupportMMLink)
          | stparse!(NativeOrigin)
          | stparse!(ImportObEditor)
          | stparse!(ImportObUpdater)
          | stparse!(ImportURL)
          | stparse!(ObjectInfo)
        ))), (props)
    );
    st!(ImportObFile, (val: data_path), (val));
    st!(ImportObFileDI, (val: data_path), (val));
    st!(ImportHint, (val: data_string), (val));
    st!(PosterFileDI, (val: data_path), (val));
    st!(BitMapDpi, (val: data_integer), (val));
    st!(ImportObFixedSize, (val: data_boolean), (val));
    st!(FlipLR, (val: data_boolean), (val));
    st!(ImportObNameDI, (val: data_path), (val));
    st!(ObjectActivateInPDF, (val: data_boolean), (val));
    st!(ObjectOpenInFloatWindow, (val: data_boolean), (val));
    st!(ObjectSupportMMLink, (val: data_boolean), (val));
    st!(NativeOrigin, (val: data_X_Y), (val));
    st!(ImportObEditor, (val: data_string), (val));
    st!(ImportObUpdater, (val: data_string), (val));
    st!(ImportURL, (val: data_string), (val));
    st!(ObjectInfo, (val: data_string), (val));
    st!(PolyLine,
        (props: many0!(alt_complete!(
            generic_object_statements!()
          | stparse!(HeadCap)
          | stparse!(TailCap)
          | stparse!(ArrowStyle)
          | stparse!(Smoothed)
          | stparse!(NumPoints)
          | stparse!(Point)
        ))), (props)
    );
    st!(HeadCap, (val: mif_parse_keyword), (val));
    st!(TailCap, (val: mif_parse_keyword), (val));
    st!(ArrowStyle,
        (props: many0!(alt_complete!(
            stparse!(TipAngle)
          | stparse!(BaseAngle)
          | stparse!(Length)
          | stparse!(HeadType)
          | stparse!(ScaleHead)
          | stparse!(ScaleFactor)
        ))), (props)
    );
    st!(TipAngle, (val: data_integer), (val));
    st!(BaseAngle, (val: data_integer), (val));
    st!(Length, (val: data_dimension), (val));
    st!(HeadType, (val: mif_parse_keyword), (val));
    st!(ScaleHead, (val: data_boolean), (val));
    st!(ScaleFactor, (val: data_dimension), (val));
    st!(NumPoints, (val: data_integer), (val));
    st!(Point, (val: data_X_Y), (val));
    st!(Rectangle,
        (props: many0!(alt_complete!(
            generic_object_statements!()
          | stparse!(ShapeRect)
          | stparse!(BRect)
          | stparse!(Smoothed)
        ))), (props)
    );
    st!(TextRect,
        (props: many0!(alt_complete!(
            generic_object_statements!()
          | stparse!(ShapeRect)
          | stparse!(BRect)
          | stparse!(TRNext)
          | stparse!(TRNumColumns)
          | stparse!(TRColumnGap)
          | stparse!(TRColumnBalance)
          | stparse!(TRSideheadWidth)
          | stparse!(TRSideheadGap)
          | stparse!(TRSideheadPlacement)
          | stparse!(TextFlow)
        ))), (props)
    );
    st!(TRNext, (val: data_integer), (val));
    st!(TRNumColumns, (val: data_integer), (val));
    st!(TRColumnGap, (val: data_dimension), (val));
    st!(TRColumnBalance, (val: data_boolean), (val));
    st!(TRSideheadWidth, (val: data_dimension), (val));
    st!(TRSideheadGap, (val: data_dimension), (val));
    // TODO: validate enum
    st!(TRSideheadPlacement, (val: mif_parse_keyword), (val));
    st!(TextLine,
        (props: many0!(alt_complete!(
            generic_object_statements!()
          | stparse!(TLOrigin)
          | stparse!(TLAlignment)
          | stparse!(TLDirection)
          | stparse!(TLLanguage)
          | stparse!(Char)
          | stparse!(Font)
          | stparse!(String)
        ))), (props)
    );
    st!(TLOrigin, (val: data_X_Y), (val));
    st!(TLAlignment, (val: mif_parse_keyword), (val));
    st!(TLDirection, (val: mif_parse_keyword), (val));
    st!(TLLanguage, (val: mif_parse_keyword), (val));

    st!(Page,
        (props: many0!(alt_complete!(
            // MIFNOTE: undocumented
            stparse!(Unique)
          | stparse!(PageType)
          | stparse!(PageNum)
          | stparse!(PageTag)
          | stparse!(PageSize)
          | stparse!(PageAngle)
          | stparse!(PageOrientation)
          | stparse!(PageBackground)
          | all_objects!()
          // TODO: filter statements?
        ))), (props)
    );
    st!(PageType, (val: mif_parse_keyword), (val));
    st!(PageNum, (val: data_string), (val));
    st!(PageTag, (val: data_tagstring), (val));
    st!(PageSize, (val: data_W_H), (val));
    st!(PageAngle, (val: data_degrees), (val));
    st!(PageOrientation, (val: mif_parse_keyword), (val));
    // MIFNOTE: documented as a keyword, actually a string
    st!(PageBackground, (val: data_string), (val));

    mynamed!(fullmiffile<Vec<MIFTree<'a>>>,
        many0!(alt_complete!(
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
          | stparse!(TextFlow)
          | stparse!(Page)
          | unknown_miftree
        ))
    );
}

// Attempt to understand the binary format
//extern crate regex;
//use std::collections::HashMap;
//use std::iter;
//use std::mem;
//use regex::Regex;
//
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
