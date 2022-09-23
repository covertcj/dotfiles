use clap::{command, Parser, Subcommand};
use git::{git_ammend_date, DateOpts};
use xshell::Shell;

mod git;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    #[command(name = "gamend")]
    GitAmend(DateOpts),
}

fn main() -> anyhow::Result<()> {
    let args = Cli::parse();

    let sh = Shell::new()?;
    match args.command {
        Commands::GitAmend(opts) => git_ammend_date(&sh, opts),
    }?;

    Ok(())
}
