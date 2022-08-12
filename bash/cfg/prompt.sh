if starship --version &> /dev/null; then
  eval "$(starship init bash)"
else
  export PS1="\[\e[32m\]\W\[\e[m\]\[\e[33m\]\\$\[\e[m\] "
fi
