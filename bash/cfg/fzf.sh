#####
# Lists all tmux sessions and attaches the selected one. An argument is treated
# as the initial filter for fzf, and solo results are automatically selected
function fzf_tmux_attach_session() {
  local session=$(tmux list-sessions -F "#{session_name}" | fzf-tmux -q "$1" 0)
  if [[ -z "$session" ]]; then
    echo 'No session found'
    return
  fi

  if [[ -z "$TMUX" ]]; then
    tmux attach -t "$session"
  else
    tmux switch-client -t "$session"
  fi
}

#####
# Lists all tmux windows and attaches the selected one. An argument is treated
# as the initial filter for fzf, and solo results are automatically selected
function fzf_tmux_attach_window() {
  local selection=$(tmux list-windows -aF "#{session_name} | #{window_name}" | fzf-tmux -a "$1" -1)
  local session=$(echo $selection | sed -E 's:([^|]*) \| (.*):\1:')
  local window=$(echo $selection | sed -E 's:([^|]*) \| (.*):\2:')
  if [[ -z "$session" ]]; then
    echo 'No session found'
    return
  fi

  tmux select-window -t "$session:$window"
  if [[ -z "$TMUX" ]]; then
    tmux attach -t "$session"
  else
    tmux switch-client -t "$session"
  fi
}

function fzf_git_checkout_branch() {
  local branch=$(git branch --sort=-committerdate --format="%(refname:short)" | fzf-tmux -q "$1" -1)
  [[ -z "$branch" ]] && return

  git checkout $branch
}

function fzf_git_checkout_branch_remote() {
  local branch=$(git branch -r --sort=-committerdate --format="%(refname:short)" | fzf-tmux -q "$1" -1)
  [[ -z "$branch" ]] && return

  local local_branch=$(echo "$branch" | sed 's/^[^/]*\///')
  git checkout $local_branch
}

