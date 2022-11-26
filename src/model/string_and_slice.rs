use std::{
    ops::{RangeFrom, RangeTo},
    str::{CharIndices, Chars},
};

use nom::{Compare, InputIter, InputLength, InputTake, Offset, UnspecializedInput};

use crate::model::slice::Slice;
use crate::utils::Loggable;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct StringAndSlice<'a> {
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
