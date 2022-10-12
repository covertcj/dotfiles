export SK_DEFAULT_BINDINGS="ctrl-y:execute-silent(echo {} | pbcopy)+abort"

#####
# Runs 'sk' with personal default settings. The first argument is passed as
# the default query, and the rest are passed along to sk
function sk_defaults() {
  sk --query="$1" --select-1 --exit-0 --bind="$SK_DEFAULT_BINDINGS" "${@:2}"
}

function sk_git_checkout_branch() {
  local branch=$(git branch --sort=-committerdate --format="%(refname:short)" | sk_defaults "$1")
  [[ -z "$branch" ]] && return

  git checkout $branch
}

function sk_git_checkout_branch_remote() {
  local branch=$(git branch -r --sort=-committerdate --format="%(refname:short)" | sk_defaults "$1")
  [[ -z "$branch" ]] && return

  local local_branch=$(echo "$branch" | sed 's/^[^/]*\///')
  git checkout $local_branch
}

#####
# Lists all tmux sessions and attaches the selected one. An argument is treated
# as  the initial filter for sk, and solo results are automatically selected
function sk_tmux_attach_session() {
  local session=$(tmux list-sessions -F "#{session_name}" | sk_defaults "$1")
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
# as  the initial filter for sk, and solo results are automatically selected
function sk_tmux_attach_window() {
  local selection=$(tmux list-windows -aF "#{session_name} | #{window_name}" | sk_defaults "$1")
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

#####
# Doesn't do anything meaningful right now, not sure I'd ever really use this
# In theory, this would list all the panes in the (current?) session and
#
function sk_tmux_switch_pane() {
  if [[ -z "$TMUX" ]]; then
    echo "not in a tmux session"
    return
  fi

  local current_pane="$(tmux display-message -p '#I:#P')"
  local current_window="$(tmux display-message -p '#I')"

  local selection=$(tmux list-panes -F "#I:#P - #{pane_current_path} - #{pane_current_command}" \
               | rg -v "$current_pane" \
               | sk_defaults "$1")

  echo "$selection"

  local pane=$(echo "$selection" | sed 's/^[0-9]*:\([0-9]*\).*/\1/')
  local window=$(echo "$selection" | sed 's/^\([0-9]*\):[0-9]*.*/\1/')

  if [[ "$current_window" -eq "$window" ]]; then
    tmux select-pane -t "$window.$pane"
  else
    tmux select-pane -t "$window.$pane" \
      && tmux select-window -t "$window"
  fi
}

#function suggest_cmd() {
#  perl -e 'ioctl STDOUT, 0x5412, $_ for split //, do { chomp($_ = <>); $_ }'
#}

#function sk_history_get() {
#  # TODO: this doesn't work yet... fzf uses some fancy zle stuff, and the perl script above doesnt seem to do anything
#  # local hist_index=$(fc -l 1 | sk_defaults "" --no-sort --tac | sed -e 's/ *\([0-9]*\) .*/\1/')
#  # echo "$index"
#
#  #$(fc -l 1 | sk_defaults "" --no-sort --tac | sed -e 's/ *[0-9]*  *//' | suggest_cmd)
#  SK_DEFAULT_BINDINGS="$SK_DEFAULT_BINDINGS,ctrl-e:execute-silent(nvim  fc -l 1 | sk_defaults "" --no-sort --tac | sed -e 's/ *[0-9]*  *//'
#}

#####
# Prompts the user to execute, edit, or cancel the given command
function confirm_and_exec() {
  local cmd="$1"
  local red='\033[0;31m'
  local green='\033[0;32m'
  local reset='\033[0m'

  echo "$cmd [${green}enter|${red}ctrl-c${reset}]"
  # TODO for some reason, the following line doesnt cause $c to equal '' or '\n' so the break doesnt work...
  while read -s -k1 c; do
    echo "$c"
    if [[ "$c" = '' ]]; then
      break
    fi

    if [[ "$c" = 'e' ]]; then
      echo 'TODO: make editing work!'
    fi
  done

  eval "$cmd"
}

#####
# List out bash history, then executes the selected result
function sk_history_exec() {
 local cmd=$(fc -l 1 | sk_defaults "" --no-sort --tac | sed -e 's/ *[0-9]*  *//')
 echo "$cmd"

 confirm_and_exec "$cmd"
}

#####
# Lists files recursively and opens the selection. The argument will be used
# as a 'fd' filter.
function sk_open_file() {
  fd "$1" | sk --bind "$SK_DEFAULT_BINDINGS,ctrl-o:execute(open {})+abort" | xargs nvim
}

