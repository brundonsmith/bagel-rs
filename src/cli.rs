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
    },

    Transpile {
        /// File or project directory
        #[arg()]
        target: String,

        #[arg(long)]
        watch: bool,
    },

    /// Typecheck and lint Bagel files
    Check {
        /// File or project directory
        #[arg()]
        target: String,

        #[arg(long)]
        watch: bool,
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
            Command::Build { target, watch } => todo!(),
            Command::Run { target, node, deno } => todo!(),
            Command::Transpile { target, watch } => todo!(),
            Command::Check { target, watch } => todo!(),
            Command::Test {
                target,
                test_filter,
                watch,
            } => todo!(),
        }
    }
}
