use std::{fmt::Write, usize};

use colored::Colorize;
use enum_variant_type::EnumVariantType;

use crate::{
    model::ast::{LocalIdentifier, ModuleID},
    model::bgl_type::SubsumationIssue,
    model::slice::Slice,
};

#[derive(Debug, Clone, PartialEq, EnumVariantType)]
pub enum BagelError {
    #[evt(derive(Debug, Clone, PartialEq))]
    ParseError {
        module_id: ModuleID,
        src: Slice,
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
        src: Slice,
        issues: SubsumationIssue,
    },
    #[evt(derive(Debug, Clone, PartialEq))]
    NotFoundError {
        module_id: ModuleID,
        identifier: LocalIdentifier,
    },
}

impl BagelError {
    pub fn pretty_print<W: Write>(&self, f: &mut W, color: bool) -> std::fmt::Result {
        match self {
            BagelError::ModuleNotFoundError {
                module_id,
                importer_module_id,
            } => todo!(),
            BagelError::ParseError {
                src,
                module_id,
                message,
            } => {
                error_heading(f, &module_id, &src, "parse error", Some(message.as_str()))?;

                f.write_char('\n')?;

                code_block_highlighted(f, &src)?;
            }
            BagelError::AssignmentError {
                module_id,
                src,
                issues,
            } => {
                error_heading(f, &module_id, &src, "assignment error", None)?;

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
                    SubsumationIssue::Mutability(destination, value) => {
                        f.write_str("Readonly type ")?;
                        f.write_str(&format!("{}", value).blue().to_string())?;
                        f.write_str(" is not assignable to mutable type ")?;
                        f.write_str(&format!("{}", destination).blue().to_string())?;
                        f.write_char('\n')?;
                    }
                };

                f.write_char('\n')?;

                code_block_highlighted(f, &src)?;
            }
            BagelError::NotFoundError {
                module_id,
                identifier,
            } => {
                error_heading(
                    f,
                    &module_id,
                    &identifier.0,
                    "unknown identifier error",
                    None,
                )?;

                f.write_fmt(format_args!(
                    " Couldn't resolve identifier {} in this scope",
                    identifier.0.as_str().blue().to_string()
                ));

                f.write_char('\n')?;
                f.write_char('\n')?;

                code_block_highlighted(f, &identifier.0)?;
            }
        };

        Ok(())
    }
}

fn error_heading<W: Write>(
    f: &mut W,
    module_id: &ModuleID,
    src: &Slice,
    kind: &str,
    message: Option<&str>,
) -> std::fmt::Result {
    // module name
    f.write_str(&format!("{}", module_id).cyan().to_string())?;

    // line and column
    f.write_char(':')?;
    let (line, column) = line_and_column(src);
    f.write_str(&line.to_string().as_str().yellow().to_string())?;
    f.write_char(':')?;
    f.write_str(&column.to_string().as_str().yellow().to_string())?;

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

fn code_block_highlighted<W: Write>(f: &mut W, highlighted_slice: &Slice) -> std::fmt::Result {
    let mut highlighted = String::with_capacity(highlighted_slice.full_string.len());
    highlighted += &highlighted_slice.full_string[0..highlighted_slice.start];
    highlighted += highlighted_slice.full_string[highlighted_slice.start..highlighted_slice.end]
        .red()
        .to_string()
        .as_str();
    highlighted += &highlighted_slice.full_string[highlighted_slice.end..];

    let mut lines_and_starts = vec![];
    let mut next_line_start = 0;
    let mut first_error_line = 0;
    let mut last_error_line = 0;
    for (line_index, line) in highlighted_slice.full_string.lines().enumerate() {
        let len = line.len();
        lines_and_starts.push(((line_index + 1).to_string(), next_line_start, line));

        if highlighted_slice.start > next_line_start {
            first_error_line = line_index;
        }

        if highlighted_slice.end > next_line_start {
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
            full_string: highlighted_slice.full_string.clone(),
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

        if line_slice.end < highlighted_slice.start || line_slice.start > highlighted_slice.end {
            f.write_str(line)?;
        } else if highlighted_slice.contains(&line_slice) {
            f.write_str(line.red().to_string().as_str())?;
        } else if line_slice.start < highlighted_slice.start {
            let src_start = highlighted_slice.start - line_slice.start;
            f.write_str(&line[0..src_start])?;
            f.write_str(&line[src_start..].red().to_string().as_str())?;
        } else {
            let src_end = highlighted_slice.end - line_slice.start;
            f.write_str(&line[0..src_end].red().to_string().as_str())?;
            f.write_str(&line[src_end..])?;
        }

        f.write_char('\n')?;
    }

    Ok(())
}

fn line_and_column(slice: &Slice) -> (usize, usize) {
    let mut line = 1;
    let mut column = 1;

    for ch in slice.full_string.chars().take(slice.start) {
        if ch == '\n' {
            line += 1;
            column = 1;
        } else {
            column += 1;
        }
    }

    (line, column)
}
