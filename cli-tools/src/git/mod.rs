use anyhow::Result;
use chrono::{Duration, Utc};
use clap::Args;
use xshell::{cmd, Shell};

const GIT_COMMITTER_DATE: &str = "GIT_COMMITTER_DATE";

#[derive(Args)]
pub struct DateOpts {
    hours: Option<i64>,

    minutes: Option<i64>,
}

// TODO: well this worked, but it also means that I rewrote my local git history
// for this repo, which isn't great...
pub fn git_ammend_date(sh: &Shell, opts: DateOpts) -> Result<()> {
    let hour_offset = Duration::hours(opts.hours.unwrap_or(0));
    let minutes_offset = Duration::hours(opts.minutes.unwrap_or(0));

    let new_date = (Utc::now() + hour_offset + minutes_offset).to_rfc2822();

    cmd!(sh, "git commit --amend --no-edit --date {new_date}")
        .env(GIT_COMMITTER_DATE, new_date)
        .run()?;

    Ok(())
}
