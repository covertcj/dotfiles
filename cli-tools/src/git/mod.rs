// TODO: I think this module makes a good candidate for a standalone cli app.
// The app would be a set of subcommands that can each launch a dedicated
// interface for interacting with git. Commands that seem obvious to me so far:
//
//   1. quick-git amend <some_opts>
//   2. quick-git spinoff feat/new_branch
//   3. quick git status  // like magit status with tab expand/collapse and
//                        // stage/unstage capability

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

pub fn git_amend_date(sh: &Shell, dry_run: bool, opts: DateOpts) -> Result<()> {
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

/// Moves local work on the current branch onto a new branch.
///
/// This is helpful when you've made commits against a shared/team branch and
/// need to move the commits to a new branch for a PR.
#[derive(Args)]
pub struct SpinoffOpts {
    /// The name of the new branch to spin off
    branch_name: String,
}

pub fn git_spinoff_branch(sh: &Shell, dry_run: bool, opts: SpinoffOpts) -> Result<()> {
    let new_branch = opts.branch_name;
    let current_branch = cmd!(sh, "git symbolic-ref -q HEAD").read()?;
    let u = "{u}";
    let upstream = cmd!(sh, "git rev-parse -q --verify @{u}").read()?;

    let switch_branch_cmd = cmd!(sh, "git switch -c {new_branch}");
    let update_ref_cmd = cmd!(
        sh,
        "git update-ref -m 'git spinoff' {current_branch} {upstream}"
    );

    if dry_run {
        println!("{}\n{}", switch_branch_cmd, update_ref_cmd);
    } else {
        switch_branch_cmd.run()?;
        update_ref_cmd.run()?;
    }

    Ok(())
}
