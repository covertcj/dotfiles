function Test-Administrator  
{  
  $user = [Security.Principal.WindowsIdentity]::GetCurrent();
  (New-Object Security.Principal.WindowsPrincipal $user).IsInRole([Security.Principal.WindowsBuiltinRole]::Administrator)  
}

if (-Not (Test-Path "~/.doom.d")) {
  if (Test-Administrator) {
    $linkSource = Join-Path -Path $PSScriptRoot -ChildPath ".doom.d/"
    New-Item -Path "$HOME/.doom.d" -ItemType SymbolicLink -Value $linkSource
  } else {
    Write-Warning "No '~/.doom.d' found, and not running as administrator. First run must be as an admin to symlink the .doom.d directory.";
    Exit 1
  }
}

if (-Not (Test-Path "~/.emacs.d")) {
  git clone --depth 1 "https://github.com/hlissner/doom-emacs" "$HOME/.emacs.d"
  Start "~/.emacs.d/bin/doom.cmd" -ArgumentList "install" -Wait -NoNewWindow
}

emacs --batch --eval "(require 'org)" --eval "(org-babel-tangle-file \`"~/dev/dotfiles/.doom.d/config.org\`")"
Start "~/.emacs.d/bin/doom.cmd" -ArgumentList "sync" -Wait -NoNewWindow

