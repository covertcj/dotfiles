;;; interactivegrep/init.el -*- lexical-binding: t; -*-
(use-package! deadgrep
  :commands (deadgrep))

(map! :leader
      :desc "deadgrep"
      "s g" #'deadgrep)
