setopt PROMPT_SUBST
export PROMPT='%B%F{red}==$(prompt_write_dir)%F{red}=>%f%b '

# finds the the closest ancestor dir that contains the given filename or dirname
function find_closest_file() {
  dir=$(pwd -P)
  while [[ ! -f "$dir/$1" && ! -d "$dir/$1" ]]; do
  # while [ ! -f "$dir/$1" ]; do
    dir=$(dirname $dir)
    if [[ "$dir" == "/" ]]; then
      return
    fi
  done

  echo "$dir"
}

function prompt_write_dir() {
  dir_chars=3

  project_dir=$(find_closest_file ".git")
  current_dir=$(pwd -P)

  if [[ -z "$project_dir" ]]; then
    if [[ $current_dir == $HOME* ]]; then
      project="~"
      current_dir=$(sed "s:^$HOME/*::" <<<$current_dir)
    else
      project="/"
      current_dir=$(sed "s:^/::" <<<$current_dir)
    fi
  else
    project=$(basename $project_dir)
    current_dir=$(sed "s:^$project_dir/*::" <<<$current_dir)
  fi 

  current_dir=$(sed "s:\([^/]\{1,$dir_chars\}\)[^/]*/:\1/:g" <<<$current_dir)

  project_text="%F{cyan}$project"

  if [[ -z "$current_dir" ]]; then
    echo "$project_text"
    return
  fi

  dir_text="%F{green}$current_dir"

  echo "$project_text $dir_text"
}

function prompt_get_project() {
  current_dir=$(pwd -P)
  if [[ "$current_dir" == "$HOME" ]]; then
    dir_text="%F{green}~"
  else
    dir_text="%F{green}$(basename $current_dir)"
  fi

  dir=$(find_closest_file ".git")
  if [[ -z "$dir" ]]; then
    echo "$dir_text"
    return
  fi 

  project=$(basename $dir)
  project_text="%F{cyan}$project"

  if [[ "$current_dir" == "$dir" ]]; then
    echo "$project_text"
    return
  fi

  echo "$project_text $dir_text"
}

