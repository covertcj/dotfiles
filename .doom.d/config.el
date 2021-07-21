(setq user-full-name "Christopher J Covert"
      user-mail-address "covertops5@gmail.com")

(defvar cjc/doom-variable-pitch-font (font-spec :family "Roboto"      :size 14 :weight 'normal))
(defvar cjc/doom-font                (font-spec :family "Roboto Mono" :size 14 :weight 'normal))
(defvar cjc/doom-big-font            (font-spec :family "Roboto Mono" :size 26 :weight 'normal))

(defvar cjc/private-init-file (concat doom-local-dir "private.el"))
(load cjc/private-init-file nil t)

; relative line numbers
(setq display-line-numbers-type t
      display-line-numbers-type 'relative)

; fullscreen by default
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(defvar cjc/default-dark-theme 'doom-challenger-deep)
(defvar cjc/default-light-theme 'doom-tomorrow-day)

(setq doom-theme cjc/default-dark-theme)

(setq cjc/theme-list
  (list cjc/default-dark-theme
        cjc/default-light-theme))
(setq cjc/theme-index 0)


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

(map! :leader
      :desc "choose theme"
      "t T" #'counsel-load-theme)

(setq doom-variable-pitch-font cjc/doom-variable-pitch-font
      doom-font                cjc/doom-font
      doom-big-font            cjc/doom-big-font)

(setq org-directory "~/org/")

(setq cjc/org-index-file (concat (file-name-as-directory    org-directory) "000 Index.org")
      cjc/org-backlog-file (concat (file-name-as-directory  org-directory) "001 Backlog.org")
      cjc/org-calendar-file (concat (file-name-as-directory org-directory) "002 Calendar.org")
      cjc/org-archive-file (concat (file-name-as-directory  org-directory) "003 Archive.org")
      cjc/org-contacts-file (concat (file-name-as-directory org-directory) "004 Contacts.org"))

(after! org
  (setq org-agenda-start-with-log-mode t
        org-log-done 'time
        org-log-into-drawer t
        org-agenda-files (list cjc/org-backlog-file
                               cjc/org-calendar-file
                               cjc/org-archive-file
                               cjc/org-contacts-file)
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@)" "|" "DONE(d!)" "CANCELLED(c@)")))

  (setq org-refile-targets
    '((cjc/org-archive-file :maxlevel . 1)))

  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (require 'org-contacts)
  (setq org-contacts-files (list cjc/org-contacts-file))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)


  (setq org-tag-alist
    '((:startgroup)
      ("home" . ?h)
      ("work" . ?w)
      ("play" . ?p)
      (:endgroup)
      ("note" . ?n)
      ("media" . ?m))))
