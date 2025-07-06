use anyhow::Result;
use clap::{Parser, Subcommand};
use std::path::PathBuf;

mod cli;
mod config;
mod exercise;
mod hint;
mod progress;
mod run;
mod verify;
mod watch;

use cli::PactlingsApp;

#[derive(Parser)]
#[command(name = "pactlings")]
#[command(about = "Interactive Pact smart contract language tutorial")]
#[command(version = "0.1.0")]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// Initialize a new pactlings workspace
    Init {
        /// Directory to initialize (default: current directory)
        #[arg(default_value = ".")]
        path: PathBuf,
    },
    /// List all exercises
    List {
        /// Show only exercises matching this filter
        #[arg(short, long)]
        filter: Option<String>,
        /// Show completed exercises
        #[arg(short, long)]
        completed: bool,
    },
    /// Run a specific exercise
    Run {
        /// Exercise name or number
        exercise: String,
    },
    /// Get a hint for an exercise
    Hint {
        /// Exercise name or number
        exercise: String,
    },
    /// Verify an exercise solution
    Verify {
        /// Exercise name or number
        exercise: String,
    },
    /// Verify all exercises
    VerifyAll,
    /// Watch for changes and auto-verify
    Watch {
        /// Exercise name or number to watch (default: current)
        exercise: Option<String>,
    },
    /// Reset an exercise to its initial state
    Reset {
        /// Exercise name or number
        exercise: String,
    },
    /// Show progress statistics
    Progress,
    /// Run the Pact REPL with tutorial context
    Repl {
        /// Load specific exercise context
        #[arg(short, long)]
        exercise: Option<String>,
    },
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();
    let mut app = PactlingsApp::new()?;

    match cli.command {
        Some(Commands::Init { path }) => app.init(path).await,
        Some(Commands::List { filter, completed }) => app.list(filter, completed).await,
        Some(Commands::Run { exercise }) => app.run(exercise).await,
        Some(Commands::Hint { exercise }) => app.hint(exercise).await,
        Some(Commands::Verify { exercise }) => app.verify(exercise).await,
        Some(Commands::VerifyAll) => app.verify_all().await,
        Some(Commands::Watch { exercise }) => app.watch(exercise).await,
        Some(Commands::Reset { exercise }) => app.reset(exercise).await,
        Some(Commands::Progress) => app.progress().await,
        Some(Commands::Repl { exercise }) => app.repl(exercise).await,
        None => app.interactive_mode().await,
    }
}