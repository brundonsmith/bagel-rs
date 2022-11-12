use std::{
    fmt::Debug,
    ops::{Index, Range, RangeBounds, RangeFrom, RangeTo},
    slice::SliceIndex,
    str::{CharIndices, Chars},
};

use nom::{
    Compare, InputIter, InputLength, InputTake, InputTakeAtPosition, Offset, UnspecializedInput,
};

#[derive(Clone, PartialEq, Copy)]
pub struct Slice<'a> {
    string: &'a str,
    start: usize,
    end: usize,
}

impl<'a> Slice<'a> {
    pub fn new(string: &'a str) -> Self {
        Self {
            string,
            start: 0,
            end: string.len(),
        }
    }
}

impl<'a> Debug for Slice<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Slice<")?;
        f.write_fmt(format_args!("{:?}", self.start))?;
        f.write_str("..")?;
        f.write_fmt(format_args!("{:?}", self.end))?;
        f.write_str("|")?;
        f.write_fmt(format_args!("{:?}", self.as_str()))?;
        f.write_str(">")
    }
}

impl<'a> From<Slice<'a>> for &'a str {
    fn from(value: Slice<'a>) -> Self {
        &value.string[value.start..value.end]
    }
}

impl<'a> Slice<'a> {
    pub fn contains(&'a self, other: Slice<'a>) -> bool {
        assert_eq!(self.string, other.string);

        self.start <= other.start && self.end >= other.end
    }

    pub fn spanning(self, other: Slice<'a>) -> Slice<'a> {
        assert_eq!(self.string, other.string);

        Self {
            string: self.string,
            start: usize::min(self.start, other.start),
            end: usize::max(self.end, other.end),
        }
    }

    pub fn as_str(self) -> &'a str {
        self.into()
    }

    pub fn len(&self) -> usize {
        self.end - self.start
    }

    pub fn trim_start(&self) -> Slice<'a> {
        let s = self.as_str();
        let self_len = s.len();
        let trimmed_len = self_len - s.trim_start().len();

        Self {
            string: self.string,
            start: self.start + trimmed_len,
            end: self.end,
        }
    }

    pub fn trim_start_matches<F: Fn(char) -> bool>(&self, pat: F) -> Slice<'a> {
        let s = self.as_str();
        let self_len = s.len();
        let trimmed_len = self_len - s.trim_start_matches(pat).len();

        Self {
            string: self.string,
            start: self.start + trimmed_len,
            end: self.end,
        }
    }

    pub fn starts_with(&self, prefix: &str) -> bool {
        self.as_str().starts_with(prefix)
    }

    pub fn slice_range(self, start: usize, end: Option<usize>) -> Slice<'a> {
        Self {
            string: self.string,
            start: self.start + start,
            end: end.map(|end| self.start + end).unwrap_or(self.end),
        }
    }
}

impl<'a> InputLength for Slice<'a> {
    fn input_len(&self) -> usize {
        self.as_str().input_len()
    }
}

impl<'a> InputTake for Slice<'a> {
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

impl<'a> InputIter for Slice<'a> {
    type Item = char;
    type Iter = CharIndices<'a>;
    type IterElem = Chars<'a>;

    fn iter_indices(&self) -> Self::Iter {
        self.as_str().iter_indices()
    }

    fn iter_elements(&self) -> Self::IterElem {
        self.as_str().iter_elements()
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

impl<'a> UnspecializedInput for Slice<'a> {}

impl<'a> Offset for Slice<'a> {
    fn offset(&self, second: &Self) -> usize {
        second.start - self.start
    }
}

impl<'a> nom::Slice<RangeFrom<usize>> for Slice<'a> {
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        Self {
            string: self.string,
            start: self.start + range.start,
            end: self.end,
        }
    }
}

impl<'a> nom::Slice<RangeTo<usize>> for Slice<'a> {
    fn slice(&self, range: RangeTo<usize>) -> Self {
        Self {
            string: self.string,
            start: self.start,
            end: self.start + range.end,
        }
    }
}

impl<'a> Compare<Slice<'a>> for Slice<'a> {
    fn compare(&self, t: Slice<'a>) -> nom::CompareResult {
        self.as_str().compare(t.as_str())
    }

    fn compare_no_case(&self, t: Slice<'a>) -> nom::CompareResult {
        self.as_str().compare_no_case(t.as_str())
    }
}

impl<'a> Compare<&'a str> for Slice<'a> {
    fn compare(&self, t: &'a str) -> nom::CompareResult {
        self.as_str().compare(t)
    }

    fn compare_no_case(&self, t: &'a str) -> nom::CompareResult {
        self.as_str().compare_no_case(t)
    }
}

#[test]
fn foo() {
    println!(
        "{:?}",
        (
            Slice::new("foo bar").take_split(2).0.as_str(),
            Slice::new("foo bar").take_split(2).1.as_str()
        )
    );
    println!("{:?}", "foo bar".take_split(2));
}
