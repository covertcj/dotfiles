;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Christopher J Covert"
      user-mail-address "covertops5@gmail.com")

(defvar cjc/doom-variable-pitch-font (font-spec :family "Iosevka Term SS12" :size 15 :weight 'normal))
(defvar cjc/doom-font                (font-spec :family "Iosevka Term SS12" :size 15 :weight 'normal))
(defvar cjc/doom-big-font            (font-spec :family "Iosevka Term SS12" :size 26 :weight 'normal))

(defvar cjc/theme-dark-default 'doom-monokai-pro)
(defvar cjc/theme-light-default 'doom-solarized-light)
  (setq cjc/theme-list
    (list cjc/theme-dark-default
	        cjc/theme-light-default))

(setq display-line-numbers-type 'relative)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(setq doom-font-increment 1)
(setq doom-variable-pitch-font cjc/doom-variable-pitch-font
      doom-font                cjc/doom-font
      doom-big-font            cjc/doom-big-font)

(setq doom-themes-enable-italic nil)

(setq cjc/theme-index 0)
(setq doom-theme (car cjc/theme-list))
(defun cjc/toggle-themes ()
  "Switches between a list of themes"
  (interactive)
  (let* ((current-theme (nth cjc/theme-index cjc/theme-list))
         (next-index (mod (+ cjc/theme-index 1) (length cjc/theme-list)))
         (next-theme (nth next-index cjc/theme-list)))
    (disable-theme current-theme)
    (message "Theme: %s" next-theme)
    (setq cjc/theme-index next-index)
    (condition-case nil
        (enable-theme next-theme)
      (error (load-theme next-theme t)))))

(map! :leader
      :desc "toggle theme"
      "t t" #'cjc/toggle-themes)

(setq doom-ir-black-brighter-comments t)

(setq org-directory "~/org/"
      org-roam-directory "~/org/")
