use anyhow::Result;
use chrono::{Duration, Local};
use clap::Args;
use xshell::{cmd, Shell};

const GIT_COMMITTER_DATE: &str = "GIT_COMMITTER_DATE";

/// Amends the last git commit date to the current moment, but can be offset by
/// a number of hours and/or minutes.
#[derive(Args, Clone)]
pub struct DateOpts {
    // TODO: consider making these optional args like -h12 (-h-12 for negative)
    /// The number of hours to add (or subtract) from the current time
    #[arg(allow_hyphen_values = true)]
    hours: Option<i64>,

    /// The number of minutes to add (or subtract) from the current time
    #[arg(allow_hyphen_values = true)]
    minutes: Option<i64>,
}

pub fn git_ammend_date(sh: &Shell, dry_run: bool, opts: DateOpts) -> Result<()> {
    let hour_offset = Duration::hours(opts.hours.unwrap_or(0));
    let minutes_offset = Duration::minutes(opts.minutes.unwrap_or(0));

    let new_date = (Local::now() + hour_offset + minutes_offset).to_rfc2822();

    let cmd = cmd!(sh, "git commit --amend --no-edit --date {new_date}")
        .env(GIT_COMMITTER_DATE, new_date);

    if dry_run {
        println!("{}", cmd);
    } else {
        cmd.run()?;
    }

    Ok(())
}
