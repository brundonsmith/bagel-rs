pub trait Loggable: std::fmt::Debug + Sized {
    fn log(self) -> Self {
        println!("{:?}", &self);
        self
    }
}
