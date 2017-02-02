// TODO: use Stream in this module rather than &[u8]
// TODO: allow \r in line endings
// TODO: allow tabs as whitespace
// TODO: allow no trailing newline

use std::str::from_utf8;

use combine::Parser;
use combine::between;
use combine::choice;
use combine::look_ahead;
use combine::many;
use combine::many1;
use combine::optional;
use combine::satisfy;
use combine::skip_many;
use combine::try;
use combine::unexpected;

use combine::primitives::ParseResult;

use combine::byte::byte;

use combine::range::take;
use combine::range::take_while;
use combine::range::take_while1;

use super::briefer_utf8;

use self::MIFToken::*;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum MIFToken<'a> {
    StartStatement,
    EndStatement,
    Str(&'a str),
    Token(&'a str),
    Inset { ty: &'a str, content: &'a str },
}

pub fn lex(mif: &[u8]) -> Vec<MIFToken>{
    match alltokens(mif) {
        Ok((tokentree, consumed)) => {
            let unparsed = consumed.as_ref();
            if unparsed.len() != 0 {
                println!("DONE: {:?}\n\n\nunparsed:{:?}", tokentree, briefer_utf8(unparsed));
                panic!()
            }
            tokentree
        },
        Err(consumed) => {
            let err = consumed.into_inner();
            let err = err.translate_position(mif);
            println!("ERROR tokenizing: {:?}\n\n\nfail:{:?}", err, briefer_utf8(&mif[err.position..]));
            panic!()
        }
    }
}

macro_rules! c {
    ($x:expr) => { $x.iter().cloned() };
}

fn skipblanklines(input: &[u8]) -> ParseResult<(), &[u8]> {
    skip_many(
        try(
            (skipspaces as fn(_) -> _)
            .and(skipcomment as fn(_) -> _)
            .and(byte(b'\n'))
        )
    ).parse_stream(input)
}
fn skipspaces(input: &[u8]) -> ParseResult<(), &[u8]> {
    take_while(|b| b == b' ')
        .map(|_| ()).parse_stream(input)
}
fn skipcomment(input: &[u8]) -> ParseResult<(), &[u8]> {
    optional(byte(b'#').and(take_while(|b| b != b'\n')))
        .map(|_| ()).parse_stream(input)
}

const L_QUOTE: u8 = b'\x60'; // '
const R_QUOTE: u8 = b'\x27'; // `
fn is_elt_sep(b: u8) -> bool {
    [b'\n', b' ', b'<', b'>'].contains(&b)
}

macro_rules! mkparser {
    ($name:ident, $code:tt) => {
        fn $name(input: &[u8]) -> ParseResult<MIFToken, &[u8]> {
            $code.parse_stream(input)
        }
    }
}
mkparser!(mif_startstatement, { byte(b'<').map(|_| StartStatement) });
mkparser!(mif_endstatement, { byte(b'>').map(|_| EndStatement) });
mkparser!(mif_string, {
    between(
        byte(L_QUOTE),
        byte(R_QUOTE),
        take_while(|b| b != R_QUOTE).map(|bs| Str(from_utf8(bs).unwrap()))
    )
        .skip(look_ahead(satisfy(is_elt_sep)))
});
fn is_token_part(b: u8) -> bool {
    b == b'.' ||
    b == b'_' ||
    b == b'%' ||
    b == b'-' ||
    (b'0' <= b && b <= b'9') ||
    (b'a' <= b && b <= b'z') ||
    (b'A' <= b && b <= b'Z')
}
mkparser!(mif_token, {
    take_while1(is_token_part).map(|bs| Token(from_utf8(bs).unwrap()))
        .skip(look_ahead(satisfy(is_elt_sep)))
});
// Must *only* be used when you know it's the start of a line
// TODO: this shouldn't return a vec, we just do it to make typeck easy
fn mif_inset(input: &[u8]) -> ParseResult<Vec<MIFToken>, &[u8]> {
    fn mif_untilendinset(input: &[u8]) -> ParseResult<&[u8], &[u8]> {
        const ENDSTR: &'static [u8] = b"\n=EndInset\n";
        let endpos = input.windows(ENDSTR.len()).position(|bs| bs == ENDSTR);
        if let Some(endpos) = endpos {
            take(endpos+1).skip(take(ENDSTR.len()-1)).parse_stream(input)
        } else {
            unexpected("No end to inset").map(|_| &[][..]).parse_stream(input)
        }
    }
    between(byte(b'='), byte(b'\n'), take_while1(|b| b != b'\n'))
        .and(mif_untilendinset as fn(_) -> _)
        .map(|(ty, content)|
            (from_utf8(ty).unwrap(), from_utf8(content).unwrap()))
        .map(|(ty, content)| vec![Inset { ty, content }])
        .parse_stream(input)
}

fn alltokens<'a>(input: &'a [u8]) -> ParseResult<Vec<MIFToken<'a>>, &[u8]> {
    let skipblanklines = skipblanklines as fn(_) -> _;
    let mut parser = many(
        try(skipblanklines.with(
            (mif_inset as fn(_) -> _).or(
                tokenline as fn(_) -> _
            )
        ))
    )
        .skip(skipblanklines)
        .map(|mifss: Vec<Vec<MIFToken<'a>>>|
            mifss.into_iter().flat_map(|mifs| mifs.into_iter()).collect()
        );
    parser.parse_stream(input)
}
fn tokenline(input: &[u8]) -> ParseResult<Vec<MIFToken>, &[u8]> {
    let skipspaces = skipspaces as fn(_) -> _;
    many1(
        skipspaces.with(choice([
            try(mif_startstatement as fn(_) -> _),
            try(mif_endstatement),
            try(mif_string),
            try(mif_token),
        ])).skip(skipspaces).skip(skipcomment as fn(_) -> _)
    ).skip(byte(b'\n')).parse_stream(input)
}
