use std::{fmt::Write, usize};

use colored::Colorize;
use enum_variant_type::EnumVariantType;

use crate::{
    ast::{LocalIdentifier, ModuleID, Src},
    bgl_type::SubsumationIssue,
    slice::Slice,
    string_and_slice::StringAndSlice,
};

#[derive(Debug, Clone, PartialEq, EnumVariantType)]
pub enum BagelError {
    #[evt(derive(Debug, Clone, PartialEq))]
    ParseError {
        module_id: ModuleID,
        module_src: String,
        index: Option<usize>,
        message: String,
    },
    #[evt(derive(Debug, Clone, PartialEq))]
    ModuleNotFoundError {
        module_id: ModuleID,
        importer_module_id: Option<ModuleID>,
    },
    #[evt(derive(Debug, Clone, PartialEq))]
    AssignmentError {
        module_id: ModuleID,
        src: Option<Slice>,
        issues: SubsumationIssue,
    },
    #[evt(derive(Debug, Clone, PartialEq))]
    NotFoundError {
        module_id: ModuleID,
        identifier: Src<LocalIdentifier>,
    },
}

impl BagelError {
    pub fn pretty_print<W: Write>(
        self,
        f: &mut W,
        module_src: &str,
        color: bool,
    ) -> std::fmt::Result {
        match self {
            BagelError::ModuleNotFoundError {
                module_id,
                importer_module_id,
            } => todo!(),
            BagelError::ParseError {
                module_src,
                index,
                module_id,
                message,
            } => {
                pretty_print_parse_error(
                    &ParseError {
                        module_id,
                        module_src,
                        index,
                        message,
                    },
                    f,
                    color,
                )?;
            }
            BagelError::AssignmentError {
                module_id,
                src,
                issues,
            } => {
                error_heading(
                    f,
                    &module_src,
                    &module_id,
                    src.map(|s| s.start),
                    "assignment error",
                    None,
                )?;

                match issues {
                    SubsumationIssue::Assignment(levels) => {
                        for (index, (destination, value)) in levels.into_iter().enumerate() {
                            for _ in 0..index + 1 {
                                f.write_char(' ')?;
                            }

                            f.write_str("Type ")?;
                            f.write_str(&format!("{}", value).blue().to_string())?;
                            f.write_str(" is not assignable to type ")?;
                            f.write_str(&format!("{}", destination).blue().to_string())?;
                            f.write_char('\n')?;
                        }
                    }
                };

                f.write_char('\n')?;

                if let Some(src) = src {
                    code_block_highlighted(f, module_src, src)?;
                }
            }
            BagelError::NotFoundError {
                module_id,
                identifier,
            } => todo!(),
        };

        Ok(())
    }
}

pub fn pretty_print_parse_error<W: Write>(
    err: &ParseError,
    f: &mut W,
    color: bool,
) -> std::fmt::Result {
    error_heading(
        f,
        &err.module_src,
        &err.module_id,
        err.index,
        "parse error",
        Some(err.message.as_str()),
    )?;

    if let Some(index) = err.index {
        f.write_char('\n')?;

        code_block_highlighted(
            f,
            &err.module_src,
            Slice {
                start: index,
                end: err.module_src.len(),
            },
        )?;
    }

    Ok(())
}

fn error_heading<W: Write>(
    f: &mut W,
    module_src: &str,
    module_id: &ModuleID,
    index: Option<usize>,
    kind: &str,
    message: Option<&str>,
) -> std::fmt::Result {
    // module name
    f.write_str(&format!("{}", module_id).cyan().to_string())?;

    // line and column
    if let Some(index) = index {
        f.write_char(':')?;
        let (line, column) = line_and_column(module_src, index);
        f.write_str(&line.to_string().as_str().yellow().to_string())?;
        f.write_char(':')?;
        f.write_str(&column.to_string().as_str().yellow().to_string())?;
    }

    f.write_str(" - ")?;

    // error type
    f.write_str(&kind.red().to_string())?;

    // details
    if let Some(message) = message {
        f.write_char(' ')?;
        f.write_str(message)?;
    }

    f.write_char('\n')?;

    Ok(())
}

fn code_block_highlighted<W: Write>(f: &mut W, module_src: &str, src: Slice) -> std::fmt::Result {
    let mut highlighted = String::with_capacity(module_src.len());
    highlighted += &module_src[0..src.start];
    highlighted += module_src[src.start..src.end].red().to_string().as_str();
    highlighted += &module_src[src.end..];

    let mut lines_and_starts = vec![];
    let mut next_line_start = 0;
    let mut first_error_line = 0;
    let mut last_error_line = 0;
    for (line_index, line) in module_src.lines().enumerate() {
        let len = line.len();
        lines_and_starts.push(((line_index + 1).to_string(), next_line_start, line));

        if src.start > next_line_start {
            first_error_line = line_index;
        }

        if src.end > next_line_start {
            last_error_line = line_index;
        }

        next_line_start += len + 1;
    }
    let widest_line_number = lines_and_starts
        .iter()
        .map(|(line_number, _, _)| line_number.len())
        .fold(0, usize::max);

    let first_displayed_line = if first_error_line > 0 {
        first_error_line - 1
    } else {
        first_error_line
    };

    let last_displayed_line = if last_error_line < lines_and_starts.len() - 1 {
        last_error_line + 1
    } else {
        last_error_line
    };

    for (line_number, line_start_index, line) in lines_and_starts
        .into_iter()
        .skip(first_displayed_line)
        .take(last_displayed_line + 1 - first_displayed_line)
    {
        let line_slice = Slice {
            start: line_start_index,
            end: line_start_index + line.len(),
        };

        // line number
        f.write_char(' ')?;
        f.write_str(
            &" ".repeat(widest_line_number - &line_number.len())
                .black()
                .on_white()
                .to_string(),
        )?;
        f.write_str(&line_number.black().on_white().to_string())?;
        f.write_str("  ")?;

        if line_slice.end < src.start || line_slice.start > src.end {
            f.write_str(line)?;
        } else if src.contains(&line_slice) {
            f.write_str(line.red().to_string().as_str())?;
        } else if line_slice.start < src.start {
            let src_start = src.start - line_slice.start;
            f.write_str(&line[0..src_start])?;
            f.write_str(&line[src_start..].red().to_string().as_str())?;
        } else {
            let src_end = src.end - line_slice.start;
            f.write_str(&line[0..src_end].red().to_string().as_str())?;
            f.write_str(&line[src_end..])?;
        }

        f.write_char('\n')?;
    }

    Ok(())
}

fn line_and_column(s: &str, index: usize) -> (usize, usize) {
    let mut line = 1;
    let mut column = 1;

    for ch in s.chars().take(index) {
        if ch == '\n' {
            line += 1;
            column = 1;
        } else {
            column += 1;
        }
    }

    (line, column)
}
