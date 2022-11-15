use std::fmt::Debug;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Slice {
    pub start: usize,
    pub end: usize,
}

impl Slice {
    pub fn new(string: &str) -> Self {
        Self {
            start: 0,
            end: string.len(),
        }
    }
}

impl Slice {
    pub fn contains(&self, other: &Slice) -> bool {
        self.start <= other.start && self.end >= other.end
    }

    pub fn contains_index(&self, index: usize) -> bool {
        index >= self.start && index <= self.end
    }

    pub fn overlaps(&self, other: &Slice) -> bool {
        self.contains(other) || self.contains_index(other.start) != self.contains_index(other.end)
    }

    pub fn spanning(&self, other: &Slice) -> Slice {
        Self {
            start: usize::min(self.start, other.start),
            end: usize::max(self.end, other.end),
        }
    }

    pub fn of_str<'a>(&self, s: &'a str) -> &'a str {
        &s[self.start..self.end]
    }

    pub fn slice_range(&self, start: usize, end: Option<usize>) -> Slice {
        Self {
            start: self.start + start,
            end: end.map(|end| self.start + end).unwrap_or(self.end),
        }
    }
}

#[test]
fn slice_range() {
    let slice = Slice { start: 0, end: 12 };

    let slice = slice.slice_range(4, None);
    assert_eq!(slice, Slice { start: 4, end: 12 });

    let slice = slice.slice_range(0, None);
    assert_eq!(slice, Slice { start: 4, end: 12 });

    let slice = slice.slice_range(0, Some(6));
    assert_eq!(slice, Slice { start: 4, end: 10 });

    let slice = slice.slice_range(1, Some(2));
    assert_eq!(slice, Slice { start: 5, end: 6 });
}
