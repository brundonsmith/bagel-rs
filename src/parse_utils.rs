use crate::slice::Slice;

#[derive(Clone, Debug, PartialEq)]
pub struct PartialParseError<'a> {
    pub src: Slice<'a>,
    pub message: String,
}

pub fn parse_str<'a>(src: Slice<'a>, segment: &str) -> ParseResult<'a, Slice<'a>> {
    if src.starts_with(segment) {
        Ok((
            src.slice_range(segment.len(), None),
            src.slice_range(0, Some(segment.len())),
        ))
    } else {
        Err(None)
    }
}

pub fn parse_whitespace<'a>(src: Slice<'a>) -> ParseResult<'a, ()> {
    Ok((src.trim_start(), ()))
}

pub fn parse_optional<'a, R, F: Fn(Slice<'a>) -> ParseResult<'a, R>>(
    src: Slice<'a>,
    item: F,
) -> ParseResult<'a, Option<R>> {
    let parsed = item(src);

    if let Err(None) = parsed {
        Ok((src, None))
    } else {
        parsed.map(|(src, parsed)| (src, Some(parsed)))
    }
}

pub fn parse_series<'a, 'b, R, F: Fn(Slice<'a>) -> ParseResult<'a, R>>(
    src: Slice<'a>,
    item: F,
    delimiters: &'b [&'static str],
    ParseSeriesOptions {
        leading_delimiter,
        trailing_delimiter,
        whitespace_forbidden,
        min_items,
    }: ParseSeriesOptions,
) -> ParseResult<'a, ParsedSeries<'a, R>> {
    let mut src = src;
    let mut series = ParsedSeries {
        leading_delimiter: None,
        items: vec![],
    };

    loop {
        // Attempt to parse the next item
        match item(src) {
            Ok((new_src, item)) => {
                src = new_src;

                // skip whitespace
                src = parse_whitespace(src).map(|(src, _)| src).unwrap_or(src);

                // parse delimiter
                let (new_src, delimiter) = one_of(
                    src,
                    delimiters.into_iter().map(|delimiter| {
                        boxed_parse_function(move |src| parse_str(src, delimiter))
                    }),
                )?;
                src = new_src;
                series.items.push((item, Some(delimiter)));
            }
            Err(Some(err)) => return Err(Some(err)),
            Err(None) => {
                if series.items.len() < min_items {
                    return Err(None);
                } else {
                    return Ok((src, series));
                }
            }
        };
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ParseSeriesOptions {
    pub leading_delimiter: RequiredOptionalForbidden,
    pub trailing_delimiter: RequiredOptionalForbidden,
    pub whitespace_forbidden: bool,
    pub min_items: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ParsedSeries<'a, R> {
    pub leading_delimiter: Option<Slice<'a>>,
    pub items: Vec<(R, Option<Slice<'a>>)>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum RequiredOptionalForbidden {
    Required,
    Optional,
    Forbidden,
}

pub fn boxed_parse_function<'a, T: 'a, F: 'a + Fn(Slice<'a>) -> ParseResult<'a, T>>(
    f: F,
) -> BoxedParseFunction<'a, T> {
    Box::new(f) as BoxedParseFunction<'a, T>
}

pub type ParseResult<'a, T: 'a> = Result<(Slice<'a>, T), Option<PartialParseError<'a>>>;

pub trait ParseResultImpl<'a, T> {
    fn require(self, src: Slice<'a>, description: &'a str) -> Self;

    // fn map_parsed<R, F: Fn(T) -> R>(self, f: F) -> impl ParseResultImpl<'a, R>;
}

impl<'a, T> ParseResultImpl<'a, T> for ParseResult<'a, T> {
    fn require(self, src: Slice<'a>, description: &'a str) -> Self {
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

pub type ParseFunction<'a, T: 'a> = fn(Slice<'a>) -> ParseResult<'a, T>;
pub type BoxedParseFunction<'a, T: 'a> = Box<dyn 'a + Fn(Slice<'a>) -> ParseResult<'a, T>>;

pub fn one_of<'a, T>(
    src: Slice<'a>,
    mut fns: impl 'a + Iterator<Item = BoxedParseFunction<'a, T>>,
) -> ParseResult<'a, T> {
    fns.find_map(|f| {
        let res = f(src);

        if let Err(None) = res {
            None
        } else {
            Some(res)
        }
    })
    .unwrap_or(Err(None))
}
