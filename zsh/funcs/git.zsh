# Usage: amend-git-date "$(date -v+3H -v+3M)"
function amend-git-date {
  if [ -z "$1" ]; then
    new_date=$(date)
  else
    new_date=$1
  fi

  GIT_COMMITTER_DATE="$new_date" git commit --amend --no-edit --date "$new_date"
}
