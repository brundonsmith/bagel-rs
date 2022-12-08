use colored::{Color, Colorize};

pub trait Loggable: std::fmt::Debug + Sized {
    fn log(self) -> Self {
        println!("{:?}", &self);
        self
    }
}

pub fn cli_label(s: &str, color: Color) -> String {
    format!("{: <11}", s).color(color).to_string()
}
