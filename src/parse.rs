use std::{
    collections::HashSet,
    ops::{RangeFrom, RangeTo},
    str::{CharIndices, Chars},
};

use nom::{
    branch::alt,
    bytes::complete::{escaped, tag, take_while, take_while1},
    character::complete::{alphanumeric1, char, one_of},
    combinator::{cut, fail, map, opt},
    error::{context, VerboseError, VerboseErrorKind},
    multi::separated_list0,
    sequence::{pair, preceded, separated_pair, terminated, tuple},
    Compare, IResult, InputIter, InputLength, InputTake, Offset, UnspecializedInput,
};

use crate::{
    ast::*,
    errors::{BagelError, ParseError},
    precedence::{binary_op, precedence, Assoc, Operation},
    slice::Slice,
    utils::Loggable,
};

pub fn parse(module_id: ModuleID, module_src: String) -> Result<Module, ParseError> {
    let bgl = module_src.clone();
    let bgl_and_slice = StringAndSlice {
        string: &bgl,
        slice: Slice::new(&bgl),
    };
    let res = terminated(separated_list0(whitespace, declaration), whitespace)(bgl_and_slice);

    match res {
        Ok((i, declarations)) => {
            if i.len() > 0 {
                Err(ParseError {
                    module_id,
                    index: Some(i.slice.start - bgl_and_slice.slice.start),
                    module_src,
                    message: "Failed to parse entire input".to_owned(),
                })
            } else {
                Ok(Module {
                    module_id,
                    src: module_src.clone(),
                    declarations,
                })
            }
        }
        Err(error) => {
            let errors = match error {
                nom::Err::Error(VerboseError { errors }) => Some(errors),
                nom::Err::Failure(VerboseError { errors }) => Some(errors),
                nom::Err::Incomplete(_) => None,
            };

            let info = errors
                .map(|errors| {
                    errors
                        .into_iter()
                        .filter_map(|(input, kind)| {
                            if let VerboseErrorKind::Context(context) = kind {
                                Some((input, context))
                            } else {
                                None
                            }
                        })
                        .next()
                })
                .flatten();

            Err(info
                .map(|(input, context)| ParseError {
                    module_id: module_id.clone(),
                    index: Some((input.slice.start - bgl_and_slice.slice.start) + input.len()),
                    module_src: module_src.clone(),
                    message: format!("Failed while parsing {}", context),
                })
                .unwrap_or_else(|| ParseError {
                    module_id: module_id.clone(),
                    index: None,
                    module_src: module_src.clone(),
                    message: "Failed to parse".to_owned(),
                }))
        }
    }
}

type ParseResult<'a, T> = IResult<StringAndSlice<'a>, T, VerboseError<StringAndSlice<'a>>>;

fn declaration<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, Declaration> {
    preceded(
        whitespace,
        alt((map(value_declaration, Declaration::from),)),
    )(i)
}

fn value_declaration<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, ValueDeclaration> {
    context(
        "value declaration",
        map(
            tuple((
                alt((tag("const"), tag("let"))),
                preceded(whitespace, plain_identifier),
                preceded(whitespace, tag("=")),
                opt(preceded(whitespace, type_annotation)),
                preceded(whitespace, expression),
            )),
            |(keyword, name, _, type_annotation, value)| {
                ValueDeclaration {
                    src: value.src().map(|end| keyword.slice.spanning(&end)),
                    name,
                    type_annotation,
                    value,
                    is_const: keyword.as_str() == "const",
                    exported: false,           // TODO
                    platforms: HashSet::new(), // TODO
                }
            },
        ),
    )(i)
}

fn type_annotation<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, TypeExpression> {
    preceded(tag(":"), preceded(whitespace, type_expression))(i)
}

fn type_expression<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, TypeExpression> {
    todo!()
}

fn expression<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, Expression> {
    preceded(whitespace, alt((binary_operation, atomic_expression)))(i)
}

fn atomic_expression<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, Expression> {
    preceded(
        whitespace,
        alt((
            map(parenthesis, Expression::from),
            map(object_literal, Expression::from),
            map(array_literal, Expression::from),
            map(exact_string_literal, Expression::from),
            map(number_literal, Expression::from),
            map(boolean_literal, Expression::from),
            map(nil_literal, Expression::from),
            map(local_identifier, Expression::from),
        )),
    )(i)
}

#[test]
fn dfslkughsdfg() {
    let s = String::from("true");
    let s = StringAndSlice {
        string: &s,
        slice: Slice {
            start: 0,
            end: s.len(),
        },
    };

    println!("{:?}", atomic_expression(s));
}

fn binary_operation<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, Expression> {
    context(
        "binary operation",
        precedence(
            fail,
            // unary_op(1, tag("-")),
            fail,
            alt((
                binary_op(
                    2,
                    Assoc::Left,
                    map(preceded(whitespace, tag("*")), |op| {
                        BinaryOperator::from_symbol(op.as_str())
                    }),
                ),
                binary_op(
                    2,
                    Assoc::Left,
                    map(preceded(whitespace, tag("/")), |op| {
                        BinaryOperator::from_symbol(op.as_str())
                    }),
                ),
                binary_op(
                    3,
                    Assoc::Left,
                    map(preceded(whitespace, tag("+")), |op| {
                        BinaryOperator::from_symbol(op.as_str())
                    }),
                ),
                binary_op(
                    3,
                    Assoc::Left,
                    map(preceded(whitespace, tag("-")), |op| {
                        BinaryOperator::from_symbol(op.as_str())
                    }),
                ),
            )),
            atomic_expression,
            |op: Operation<BinaryOperator, BinaryOperator, BinaryOperator, Expression>| match op {
                // Operation::Prefix("-", o) => Ok(-o),
                Operation::Binary(left, op, right) => Ok(Expression::BinaryOperation {
                    src: left.spanning(&right),
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                }),
                // Operation::Binary(lhs, "*", rhs) => Ok(lhs * rhs),
                // Operation::Binary(lhs, "/", rhs) => Ok(lhs / rhs),
                // Operation::Binary(lhs, "+", rhs) => Ok(lhs + rhs),
                // Operation::Binary(lhs, "-", rhs) => Ok(lhs - rhs),
                _ => Err("Invalid combination"),
            },
        ),
    )(i)
}

/// fn parser(i: &str) -> IResult<&str, i64> {
///   precedence(
///     unary_op(1, tag("-")),
///     fail,
///     alt((
///       binary_op(2, Assoc::Left, tag("*")),
///       binary_op(2, Assoc::Left, tag("/")),
///       binary_op(3, Assoc::Left, tag("+")),
///       binary_op(3, Assoc::Left, tag("-")),
///     )),
///     alt((
///       map_res(digit1, |s: &str| s.parse::<i64>()),
///       delimited(tag("("), parser, tag(")")),
///     )),
///     |op: Operation<&str, &str, &str, i64>| {
///       use nom_7_precedence::Operation::*;
///       match op {
///         Prefix("-", o) => Ok(-o),
///         Binary(lhs, "*", rhs) => Ok(lhs * rhs),
///         Binary(lhs, "/", rhs) => Ok(lhs / rhs),
///         Binary(lhs, "+", rhs) => Ok(lhs + rhs),
///         Binary(lhs, "-", rhs) => Ok(lhs - rhs),
///         _ => Err("Invalid combination"),
///       }
///     }
///   )(i)
/// }

// + ContextError<Slice>
fn parenthesis<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, Parenthesis> {
    context(
        "parenthesized expression",
        map(
            pair(
                tag("("),
                cut(pair(
                    preceded(whitespace, expression),
                    preceded(whitespace, tag(")")),
                )),
            ),
            |(open_paren, (inner, close_paren))| Parenthesis {
                src: Some(open_paren.slice.spanning(&close_paren.slice)),
                inner: Box::new(inner),
            },
        ),
    )(i)
}

fn object_literal<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, ObjectLiteral> {
    context(
        "object",
        map(
            pair(
                tag("{"),
                cut(pair(
                    separated_list0(preceded(whitespace, char(',')), key_value),
                    preceded(whitespace, tag("}")),
                )),
            ),
            |(open_bracket, (entries, close_bracket))| ObjectLiteral {
                src: Some(open_bracket.slice.spanning(&close_bracket.slice)),
                entries: entries
                    .into_iter()
                    .map(|(key, value)| {
                        ObjectLiteralEntry::KeyValue(
                            PlainIdentifier {
                                src: Some(key.slice),
                                name: key.as_str().to_owned(),
                            },
                            value,
                        )
                    })
                    .collect(),
            },
        ),
    )(i)
}

fn key_value<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, (StringAndSlice<'a>, Expression)> {
    separated_pair(
        preceded(whitespace, identifier_like),
        cut(preceded(whitespace, char(':'))),
        expression,
    )(i)
}

fn array_literal<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, ArrayLiteral> {
    context(
        "array",
        map(
            pair(
                tag("["),
                cut(pair(
                    separated_list0(preceded(whitespace, char(',')), expression),
                    preceded(whitespace, tag("]")),
                )),
            ),
            |(open_bracket, (entries, close_bracket))| ArrayLiteral {
                src: Some(open_bracket.slice.spanning(&close_bracket.slice)),
                entries: entries
                    .into_iter()
                    .map(ArrayLiteralEntry::Element)
                    .collect(),
            },
        ),
    )(i)
}

fn exact_string_literal<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, ExactStringLiteral> {
    context(
        "string",
        map(
            pair(tag("\'"), cut(pair(string_contents, tag("\'")))),
            |(open_quote, (contents, close_quote))| ExactStringLiteral {
                src: Some(open_quote.slice.spanning(&close_quote.slice)),
                tag: None, // TODO
                value: contents.as_str().to_owned(),
            },
        ),
    )(i)
}

fn string_contents<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, StringAndSlice<'a>> {
    escaped(alphanumeric1, '\\', one_of("\"n\\"))(i)
}

fn number_literal<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, NumberLiteral> {
    map(
        tuple((opt(tag("-")), numeric, opt(tuple((tag("."), cut(numeric)))))),
        |(neg, int, tail)| {
            let front = neg.unwrap_or(int);
            let back = tail.map(|(_, decimal)| decimal).unwrap_or(int);
            let full = front.spanning(&back);

            NumberLiteral {
                src: Some(full.slice),
                value: full.as_str().to_owned(),
            }
        },
    )(i)
}

fn boolean_literal<'a>(input: StringAndSlice<'a>) -> ParseResult<'a, BooleanLiteral> {
    let parse_true = map(tag("true"), |src: StringAndSlice<'a>| BooleanLiteral {
        src: Some(src.slice),
        value: true,
    });

    let parse_false = map(tag("false"), |src: StringAndSlice<'a>| BooleanLiteral {
        src: Some(src.slice),
        value: false,
    });

    alt((parse_true, parse_false))(input)
}

fn nil_literal<'a>(input: StringAndSlice<'a>) -> ParseResult<'a, NilLiteral> {
    map(tag("nil"), |src: StringAndSlice<'a>| NilLiteral {
        src: Some(src.slice),
    })(input)
}

fn local_identifier<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, LocalIdentifier> {
    context(
        "identifier",
        map(identifier_like, |name| LocalIdentifier {
            src: Some(name.slice),
            name: name.as_str().to_owned(),
        }),
    )(i)
}

fn plain_identifier<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, PlainIdentifier> {
    context(
        "identifier",
        map(identifier_like, |name| PlainIdentifier {
            src: Some(name.slice),
            name: name.as_str().to_owned(),
        }),
    )(i)
}

// utils
fn identifier_like<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, StringAndSlice<'a>> {
    take_while(|ch: char| ch.is_alphanumeric() || ch == '_' || ch == '$')(i)
}

fn numeric<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, StringAndSlice<'a>> {
    take_while1(|c: char| c.is_numeric())(i)
}

fn whitespace<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, StringAndSlice<'a>> {
    take_while(|c| c == ' ' || c == '\n' || c == '\t' || c == '\r')(i)
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct StringAndSlice<'a> {
    pub string: &'a String,
    pub slice: Slice,
}

impl<'a> StringAndSlice<'a> {
    pub fn len(&self) -> usize {
        self.slice.end - self.slice.start
    }

    pub fn spanning(&self, other: &Self) -> Self {
        Self {
            string: self.string,
            slice: self.slice.spanning(&other.slice),
        }
    }

    pub fn slice_range(&self, start: usize, end: Option<usize>) -> Self {
        Self {
            string: self.string,
            slice: Slice {
                start: self.slice.start + start,
                end: end
                    .map(|end| self.slice.start + end)
                    .unwrap_or(self.slice.end),
            },
        }
    }

    pub fn as_str(&self) -> &str {
        self.slice.of_str(self.string.as_str())
    }
}

impl<'a> InputLength for StringAndSlice<'a> {
    fn input_len(&self) -> usize {
        self.as_str().input_len()
    }
}

#[test]
fn dfksjlhsdfgkh() {
    let string = String::from("abc");
    println!(
        "{}",
        StringAndSlice {
            string: &string,
            slice: Slice {
                start: 1,
                end: string.len() - 1
            }
        }
        .input_len()
    );
}

impl<'a> InputTake for StringAndSlice<'a> {
    fn take(&self, count: usize) -> Self {
        self.slice_range(0, Some(count))
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        (
            self.slice_range(count, None),
            self.slice_range(0, Some(count)),
        )
    }
}

#[test]
fn take_split() {
    let s = String::from("ksjdfg");
    let s = StringAndSlice {
        string: &s,
        slice: Slice {
            start: 0,
            end: s.len(),
        },
    };

    let index = 1;
    let (a, b) = s.take_split(index);
    assert_eq!((a.as_str(), b.as_str()), s.as_str().take_split(index));

    let index = 2;
    let (a, b) = s.take_split(index);
    assert_eq!((a.as_str(), b.as_str()), s.as_str().take_split(index));

    let s = StringAndSlice {
        string: s.string,
        slice: Slice {
            start: 2,
            end: s.len() - 1,
        },
    };

    let index = 1;
    let (a, b) = s.take_split(index);
    assert_eq!((a.as_str(), b.as_str()), s.as_str().take_split(index));

    let index = 2;
    let (a, b) = s.take_split(index);
    assert_eq!((a.as_str(), b.as_str()), s.as_str().take_split(index));
}

impl<'a> InputIter for StringAndSlice<'a> {
    type Item = char;
    type Iter = CharIndices<'a>;
    type IterElem = Chars<'a>;

    fn iter_indices(&self) -> Self::Iter {
        self.slice.of_str(self.string.as_str()).iter_indices()
    }

    fn iter_elements(&self) -> Self::IterElem {
        self.slice.of_str(self.string.as_str()).iter_elements()
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.as_str().position(predicate)
    }

    fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
        self.as_str().slice_index(count)
    }
}

impl<'a> UnspecializedInput for StringAndSlice<'a> {}

impl<'a> Offset for StringAndSlice<'a> {
    fn offset(&self, second: &Self) -> usize {
        second.slice.start - self.slice.start
    }
}

impl<'a> nom::Slice<RangeFrom<usize>> for StringAndSlice<'a> {
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        self.slice_range(range.start, None)
    }
}

impl<'a> nom::Slice<RangeTo<usize>> for StringAndSlice<'a> {
    fn slice(&self, range: RangeTo<usize>) -> Self {
        self.slice_range(self.slice.start, Some(range.end))
    }
}

impl<'a> Compare<&'a str> for StringAndSlice<'a> {
    fn compare(&self, t: &'a str) -> nom::CompareResult {
        self.as_str().compare(t)
    }

    fn compare_no_case(&self, t: &'a str) -> nom::CompareResult {
        self.as_str().compare_no_case(t)
    }
}

impl<'a> Loggable for StringAndSlice<'a> {}
