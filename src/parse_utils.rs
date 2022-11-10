#[derive(Clone, Debug, PartialEq)]
pub struct PartialParseError<'a> {
    pub src: &'a str,
    pub message: String,
}

pub fn consume<'a>(src: &'a str, segment: &str) -> ParseResult<'a, &'a str> {
    if src.starts_with(segment) {
        Ok((&src[segment.len()..], &src[0..segment.len()]))
    } else {
        Err(None)
    }
}

pub fn consume_whitespace<'a>(src: &'a str) -> ParseResult<'a, ()> {
    Ok((src.trim_start(), ()))
}

pub fn parse_series<'a, R, F: Fn(&'a str) -> ParseResult<'a, R>>(
    src: &'a str,
    item: F,
) -> ParseResult<'a, Vec<R>> {
    todo!()
}

pub fn parse_optional<'a, R, F: Fn(&'a str) -> ParseResult<'a, R>>(
    src: &'a str,
    item: F,
) -> ParseResult<'a, Option<R>> {
    todo!()
}

pub type ParseResult<'a, T> = Result<(&'a str, T), Option<PartialParseError<'a>>>;

pub trait ParseResultImpl<'a, T> {
    fn require(self, src: &'a str, description: &'a str) -> Self;

    // fn map_parsed<R, F: Fn(T) -> R>(self, f: F) -> impl ParseResultImpl<'a, R>;
}

impl<'a, T> ParseResultImpl<'a, T> for ParseResult<'a, T> {
    fn require(self, src: &'a str, description: &'a str) -> Self {
        if let Err(None) = self {
            ParseResult::Err(Some(PartialParseError {
                src,
                message: format!("Expected {}", description),
            }))
        } else {
            self
        }
    }
}

pub fn one_of<'a, 'b, const N: usize, T>(
    src: &'a str,
    fns: [fn(&'a str) -> ParseResult<'a, T>; N],
) -> ParseResult<'a, T> {
    fns.iter()
        .find_map(|f| {
            let res = f(src);

            if let Err(None) = res {
                None
            } else {
                Some(res)
            }
        })
        .unwrap_or(Err(None))
}
