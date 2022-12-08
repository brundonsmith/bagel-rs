#[derive(clap::Subcommand, Debug, Clone)]
pub enum Command {
    /// Create a new Bagel project in a new or empty directory
    New {
        /// New project directory
        #[arg()]
        dir: String,
    },

    /// Initialize a new Bagel project in the current directory
    Init,

    /// Bundle into a single JS file
    Build {
        /// Entry file or project directory
        #[arg()]
        target: String,

        #[arg(long)]
        watch: bool,

        #[arg(long)]
        clean: bool,
    },

    /// Bundle into a single JS file and immediately run it
    Run {
        /// Entry file or project directory
        #[arg()]
        target: String,

        /// Run using NodeJS
        #[arg(long)]
        node: bool,

        /// Run using Deno
        #[arg(long)]
        deno: bool,

        #[arg(long)]
        clean: bool,
    },

    Transpile {
        /// File or project directory
        #[arg()]
        target: String,

        #[arg(long)]
        watch: bool,

        #[arg(long)]
        clean: bool,
    },

    /// Typecheck and lint Bagel files
    Check {
        /// File or project directory
        #[arg()]
        target: String,

        #[arg(long)]
        watch: bool,

        #[arg(long)]
        clean: bool,
    },

    /// Run tests found in Bagel files
    Test {
        /// File or project directory
        #[arg()]
        target: String,

        /// Pattern for filtering tests by name
        #[arg()]
        test_filter: Option<String>,

        #[arg(long)]
        watch: bool,

        #[arg(long)]
        clean: bool,
    },

    Clean {
        /// File or project directory
        #[arg()]
        target: String,
    },
    // /// Reformat Bagel code
    // Format {
    //     /// File or project directory
    //     #[arg()]
    //     target: String,
    // },

    // /// Automatically fix fixable linter errors
    // Autofix {
    //     /// File or project directory
    //     #[arg()]
    //     target: String,
    // },
}

impl Command {
    pub fn perform(self) -> Result<String, String> {
        match self {
            Command::New { dir } => todo!(),
            Command::Init => todo!(),
            Command::Build {
                target,
                watch,
                clean,
            } => todo!(),
            Command::Run {
                target,
                node,
                deno,
                clean,
            } => todo!(),
            Command::Transpile {
                target,
                watch,
                clean,
            } => todo!(),
            Command::Check {
                target,
                watch,
                clean,
            } => todo!(),
            Command::Test {
                target,
                test_filter,
                watch,
                clean,
            } => todo!(),
            Command::Clean { target } => todo!(),
        }
    }
}
