;;; cjc/variables/config.el -*- lexical-binding: t; -*-

(defvar cjc/doom-variable-pitch-font (font-spec :family "Iosevka Term SS12" :size 15 :weight 'normal))
(defvar cjc/doom-font                (font-spec :family "Iosevka Term SS12" :size 15 :weight 'normal))
(defvar cjc/doom-big-font            (font-spec :family "Iosevka Term SS12" :size 26 :weight 'normal))

(defvar cjc/theme-dark-default 'doom-ir-black)
(defvar cjc/theme-light-default 'doom-solarized-light)
  (setq cjc/theme-list
    (list cjc/theme-dark-default
          cjc/theme-light-default))

(defvar cjc/org-directory "~/org/")
