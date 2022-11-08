use crate::errors::ParseError;

#[derive(Debug)]
pub enum ParseChain<'a, TCollected> {
    Parsing {
        code: &'a str,
        index: usize,
        expecting: bool,
        collected: TCollected,
    },
    None {
        index: usize,
    },
    Error(ParseError),
}

pub fn parse_chain<'a>(code: &'a str) -> ParseChain<'a, ()> {
    ParseChain::Parsing {
        code,
        index: 0,
        expecting: false,
        collected: (),
    }
}

impl<'a, TCollected> ParseChain<'a, TCollected> {
    pub fn parse<R, F: Fn(&'a str) -> ParseChain<'a, R>>(
        self,
        parse_fn: F,
    ) -> ParseChain<'a, (TCollected, R)> {
        match self {
            ParseChain::Parsing {
                code,
                index,
                collected,
                expecting,
            } => match parse_fn(code) {
                ParseChain::Parsing {
                    code,
                    index,
                    expecting,
                    collected: next,
                } => ParseChain::Parsing {
                    code,
                    index,
                    expecting,
                    collected: (collected, next),
                },
                ParseChain::None { index } => {
                    if expecting {
                        ParseChain::Error(todo!())
                    } else {
                        ParseChain::None { index }
                    }
                }
                ParseChain::Error(error) => ParseChain::Error(error),
            },
            ParseChain::None { index } => ParseChain::None { index },
            ParseChain::Error(error) => ParseChain::Error(error),
        }
    }

    pub fn parse_optional<R, F: Fn(&'a str) -> ParseChain<'a, R>>(
        self,
        parse_fn: F,
    ) -> ParseChain<'a, (TCollected, Option<R>)> {
        todo!()
    }

    pub fn parse_series<R, F: Fn(&'a str) -> ParseChain<'a, R>>(
        self,
        parse_fn: F,
        options: ParseSeriesOptions,
    ) -> ParseChain<'a, (TCollected, Vec<R>)> {
        todo!()
        // ParseChain {
        //     code: self.code,
        //     mode: self.mode,
        //     collected: self.collected.prepend(vec![]),
        // }
    }

    pub fn consume(self, s: &str) -> Self {
        if let ParseChain::Parsing {
            code,
            index,
            expecting,
            collected,
        } = self
        {
            if code.starts_with(s) {
                ParseChain::Parsing {
                    code,
                    index,
                    expecting,
                    collected,
                }
            } else {
                if expecting {
                    ParseChain::Error(todo!())
                } else {
                    ParseChain::None { index }
                }
            }
        } else {
            self
        }
    }

    pub fn consume_whitespace(self) -> Self {
        if let ParseChain::Parsing {
            code,
            index,
            expecting,
            collected,
        } = self
        {
            ParseChain::Parsing {
                code: code.trim_start(),
                index: index + (code.len() - code.trim_start().len()),
                expecting,
                collected,
            }
        } else {
            self
        }
    }

    pub fn expect(self) -> Self {
        if let ParseChain::Parsing {
            code,
            index,
            expecting,
            collected,
        } = self
        {
            ParseChain::Parsing {
                code,
                index,
                expecting: true,
                collected,
            }
        } else {
            self
        }
    }

    pub fn collect<R, F: Fn(TCollected) -> R>(self, collect_fn: F) -> ParseChain<'a, R> {
        match self {
            ParseChain::Parsing {
                code,
                index,
                expecting,
                collected,
            } => ParseChain::Parsing {
                code,
                index,
                expecting: true,
                collected: collect_fn(collected),
            },
            ParseChain::None { index } => ParseChain::None { index },
            ParseChain::Error(error) => ParseChain::Error(error),
        }
    }
}

pub struct ParseSeriesOptions<'a> {
    pub delimiter: &'a str,
}
