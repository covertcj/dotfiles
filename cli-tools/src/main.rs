use clap::{command, Parser, Subcommand};
use git::{git_amend_date, git_spinoff_branch, DateOpts, SpinoffOpts};
use xshell::Shell;

mod git;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Skips running any destructive shell commands
    #[arg(short = 'n', long)]
    dry_run: bool,

    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    #[command(name = "gamend")]
    GitAmend(DateOpts),
    #[command(name = "gspin")]
    GitSpinoff(SpinoffOpts),
}

fn main() -> anyhow::Result<()> {
    let args = Cli::parse();

    let sh = Shell::new()?;
    match args.command {
        Commands::GitAmend(opts) => git_amend_date(&sh, args.dry_run, opts),
        Commands::GitSpinoff(opts) => git_spinoff_branch(&sh, args.dry_run, opts),
    }?;

    Ok(())
}
