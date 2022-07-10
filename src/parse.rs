use std::rc::Rc;

use crate::model::{
    ast::{ASTEnum, AST},
    expressions::{BinaryOperator, Expression},
};
use chumsky::prelude::*;

pub fn parser() -> impl Parser<char, AST, Error = Simple<char>> {
    recursive(|expr| {
        let number = filter(|c: &char| c.is_ascii_digit())
            .repeated()
            .at_least(1)
            .map(|c| Expression::NumberLiteral {
                value: c.iter().cloned().collect(),
            });

        let atom = number
            .or(expr
                .delimited_by(just('('), just(')'))
                .map(|inner| Expression::Parenthesis {
                    inner: Rc::new(AST::from_node(ASTEnum::Expression(inner))),
                }))
            .padded();

        let op = |c| just(c).padded();

        let product = atom
            .clone()
            .then(
                op('*')
                    .to(BinaryOperator::Times)
                    .or(op('/').to(BinaryOperator::Divide))
                    .then(atom)
                    .repeated(),
            )
            .foldl(|left, (op, right)| Expression::BinaryOperator {
                left: Rc::new(AST::from_node(ASTEnum::Expression(left))),
                op,
                right: Rc::new(AST::from_node(ASTEnum::Expression(right))),
            });

        let sum = product
            .clone()
            .then(
                op('+')
                    .to(BinaryOperator::Plus)
                    .or(op('-').to(BinaryOperator::Minus))
                    .then(product)
                    .repeated(),
            )
            .foldl(|left, (op, right)| Expression::BinaryOperator {
                left: Rc::new(AST::from_node(ASTEnum::Expression(left))),
                op,
                right: Rc::new(AST::from_node(ASTEnum::Expression(right))),
            });

        sum
    })
    .map(|expr| AST::from_node(ASTEnum::Expression(expr)))
    .then_ignore(end())
}

// recursive(|value| {
//     let frac = just('.').chain(text::digits(10));

//     let exp = just('e')
//         .or(just('E'))
//         .chain(just('+').or(just('-')).or_not())
//         .chain(text::digits(10));

//     let number = just('-')
//         .or_not()
//         .chain(text::int(10))
//         .chain(frac.or_not().flatten())
//         .chain::<char, _, _>(exp.or_not().flatten())
//         .collect::<String>()
//         .from_str()
//         .unwrapped()
//         .labelled("number");

//     let escape = just('\\').ignore_then(
//         just('\\')
//             .or(just('/'))
//             .or(just('"'))
//             .or(just('b').to('\x08'))
//             .or(just('f').to('\x0C'))
//             .or(just('n').to('\n'))
//             .or(just('r').to('\r'))
//             .or(just('t').to('\t'))
//             .or(just('u').ignore_then(
//                 filter(|c: &char| c.is_digit(16))
//                     .repeated()
//                     .exactly(4)
//                     .collect::<String>()
//                     .validate(|digits, span, emit| {
//                         char::from_u32(u32::from_str_radix(&digits, 16).unwrap())
//                             .unwrap_or_else(|| {
//                                 emit(Simple::custom(span, "invalid unicode character"));
//                                 '\u{FFFD}' // unicode replacement character
//                             })
//                     }),
//             )),
//     );

//     let string = just('"')
//         .ignore_then(filter(|c| *c != '\\' && *c != '"').or(escape).repeated())
//         .then_ignore(just('"'))
//         .collect::<String>()
//         .labelled("string");

//     let array = value
//         .clone()
//         .chain(just(',').ignore_then(value.clone()).repeated())
//         .or_not()
//         .flatten()
//         .delimited_by(just('['), just(']'))
//         .map(Json::Array)
//         .labelled("array");

//     let member = string.clone().then_ignore(just(':').padded()).then(value);
//     let object = member
//         .clone()
//         .chain(just(',').padded().ignore_then(member).repeated())
//         .or_not()
//         .flatten()
//         .padded()
//         .delimited_by(just('{'), just('}'))
//         .collect::<HashMap<String, Json>>()
//         .map(Json::Object)
//         .labelled("object");

//     just("null")
//         .to(Json::Null)
//         .labelled("null")
//         .or(just("true").to(Json::Bool(true)).labelled("true"))
//         .or(just("false").to(Json::Bool(false)).labelled("false"))
//         .or(number.map(Json::Num))
//         .or(string.map(Json::Str))
//         .or(array)
//         .or(object)
//         .recover_with(nested_delimiters('{', '}', [('[', ']')], |_| Json::Invalid))
//         .recover_with(nested_delimiters('[', ']', [('{', '}')], |_| Json::Invalid))
//         .recover_with(skip_then_retry_until(['}', ']']))
//         .padded()
// })
// .then_ignore(end().recover_with(skip_then_retry_until([])))
