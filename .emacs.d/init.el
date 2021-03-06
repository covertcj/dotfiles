;; Font Settings
(defvar cjc/default-font-mono "Roboto Mono")
(defvar cjc/default-font-variable "Roboto")
(defvar cjc/default-font-height 150)

;; Theme Settings
(defvar cjc/default-dark-theme 'doom-challenger-deep)
(defvar cjc/default-light-theme 'doom-tomorrow-day)

;; Org Settings
(defvar cjc/default-org-notes-dir (expand-file-name "~/org/"))
(defvar cjc/default-dev-dir (expand-file-name "~/dev/dotfiles/"))
(defvar cjc/default-dotfiles-dir (concat cjc/default-dev-dir "dotfiles/"))

(defvar cjc/host (substring (shell-command-to-string "hostname") 0 -1))
(defvar cjc/hosts-dir "~/.emacs.d/hosts/")
(defvar cjc/host-init-file (concat cjc/hosts-dir cjc/host ".el"))
(defvar cjc/private-init-file "~/.emacs.d/priv.el")

(when (file-exists-p cjc/host-init-file)
  (load cjc/host-init-file))

(when (file-exists-p cjc/private-init-file)
  (load cjc/private-init-file))

;; Basic Window Settings ;;
(setq inhibit-startup-message t)

; remove unnecessary ui elements
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

; pad the edge of the window
(set-fringe-mode 10)

; no more annoying bell noises
(setq visible-bell nil)

; keep the cursor away from the edge, and don't jump around when scrolling
(setq scroll-margin 8)
(setq scroll-conservatively 1)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Line Numbers ;;
; relative line numbers everywhere
(global-display-line-numbers-mode t)
(setq display-line-numbers 'relative)

; show the cursor's current column
(column-number-mode)

; disable line numbers for certain modes
(dolist (mode '(term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Fonts ;;
(set-face-attribute 'default nil :font cjc/default-font-mono :height cjc/default-font-height)
(set-face-attribute 'fixed-pitch nil :font cjc/default-font-mono :height cjc/default-font-height)
(set-face-attribute 'variable-pitch nil :font cjc/default-font-variable :height cjc/default-font-height)

;; Setup Package Archives
(require 'package)

; setup the package archives and make sure there is a local copy
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Setup use-package ;;
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

; change the directory that most projects use for temporary/cache files
(setq user-emacs-directory (expand-file-name "~/.cache/emacs"))
(when (not (file-directory-p user-emacs-directory))
  (make-directory user-emacs-directory t))

; don't create lockfiles for modified files
(setq create-lockfiles nil)

; changes the default locations of temp/cache files for common packages
(use-package no-littering
  :config
  ; put auto-save files into the no-littering area
  (setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  ; don't put customizations into ~init.el~
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

;; Basic Keybindings ;;
; let ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general
  :config
  (general-evil-setup)
  (general-create-definer cjc/leader-key
    :states '(normal visual emacs motion)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "C-SPC"))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package hydra
  :after general)

(with-eval-after-load 'general
  (with-eval-after-load 'hydra

(cjc/leader-key
 "b"  '(:ignore t :which-key "buffers")
 "bb" '(counsel-switch-buffer :which-key "switch buffers")
 "bk" '(kill-current-buffer :which-key "kill current buffer")
 "bK" '(kill-buffer :which-key "kill buffer"))

(defhydra hydra-scale-text (:timeout 6)
  "scale editor text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

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

(cjc/leader-key
  "t" '(:ignore t :which-key "toggle settings")
  "tf" '(hydra-scale-text/body :which-key "font scaling")
  "tt" '(cjc/toggle-themes :which-key "theme")
  "tT" '(counsel-load-theme :which-key "theme"))

(defun cjc/org-open-emacs-org-file ()
  "Opens the index file from the org notes directory"
  (interactive)
  (find-file (concat cjc/dotfiles-dir "Emacs.org")))

(cjc/leader-key
  "o" '(:ignore t :which-key "open")
  "oE" '(cjc/dotfiles-dir :which-key "emacs config"))

))

(defvar after-change-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme' or `enable-theme'.")

(defadvice load-theme (after run-after-change-theme-hook activate)
  "Run `after-change-theme-hook'."
  (run-hooks 'after-change-theme-hook))

(defadvice enable-theme (after run-after-change-theme-hook activate)
  "Run `after-change-theme-hook'."
  (run-hooks 'after-change-theme-hook))

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(setq cjc/theme-list
  (list cjc/default-dark-theme
        cjc/default-light-theme))
(setq cjc/theme-index 0)

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold nil
	doom-themes-enable-italic nil)
  (load-theme (car cjc/theme-list) t)
  (doom-themes-org-config))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package hl-todo
  :config
  (defun cjc/hl-todo-setup-theme ()
    (setq hl-todo-keyword-faces
      '(("TODO" . (face-foreground 'font-lock-keyword-face))
        ("FIXME" . (face-foreground 'font-lock-keyword-face))))
    (when (or (bound-and-true-p hl-todo-mode)
              (bound-and-true-p global-hl-todo-mode))
      (hl-todo-mode)))

  (cjc/hl-todo-setup-theme)
  (add-hook 'after-change-theme-hook #'cjc/hl-todo-setup-theme t)

  (global-hl-todo-mode))

(use-package ivy
  :diminish
  :bind (:map ivy-minibuffer-map
              ("TAB" . ivy-alt-done)
              ("C-j" . ivy-next-line)
              ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
              ("C-k" . ivy-previous-line)
              ("M-k" . ivy-switch-buffer-kill))
  :config
  (setq ivy-on-del-error-function #'ignore)
  (ivy-mode 1))

(use-package counsel
  :config
  (counsel-mode 1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)))

(use-package ivy-posframe
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-bottom-left)))
  (ivy-posframe-mode 1))

(use-package all-the-icons-ivy
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

(use-package forge)
(use-package magit
  :config
  (cjc/leader-key
   "g"  '(:ignore t :which-key "git")
   "gs" '(magit-status :which-key "status")
   "gb" '(magit-branch :which-key "branch")
   "gc" '(magit-branch-or-checkout :which-key "branch or checkout")))

(when (eq system-type 'windows-nt)
  (setenv "SSH_ASKPASS" "git-gui--askpass")
  (use-package ssh-agency))

(use-package projectile
  :after (general ivy)
  :diminish projectile-mode
  :init
  (setq projectile-project-search-path '())
  (when (file-directory-p "~/dev") (push "~/dev" projectile-project-search-path))
  (when (file-directory-p "~/work") (push "~/work" projectile-project-search-path))

  (setq projectile-completion-system 'ivy)

  :config
  (projectile-mode 1)
  (cjc/leader-key
    "p" '(:keymap projectile-command-map :wk "projectile")))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :config
  (cjc/leader-key
    "l" '(:keymap lsp-command-map :wk "lsp"))
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
          ("<tab>" . company-complete-selection))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.1))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package typescript-mode
  :mode "\\.[jt]sx?\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key)
  :init
  (cjc/leader-key
     "h"  '(:ignore t :which-key "help")
     "hf" '(counsel-describe-function :which-key "functions")
     "hv" '(counsel-describe-variable :which-key "variables")
     "hc" '(helpful-command :which-key "commands")
     "hk" '(helpful-key :which-key "keys")))

(defun cjc/all-term-mode-hook ()
  (setq scroll-margin 0))

(add-hook 'eshell-mode-hook 'cjc/all-term-mode-hook)

(setq cjc/org-index-file (concat (file-name-as-directory cjc/default-org-notes-dir) "000 Index.org")
      cjc/org-backlog-file (concat (file-name-as-directory cjc/default-org-notes-dir) "001 Backlog.org")
      cjc/org-calendar-file (concat (file-name-as-directory cjc/default-org-notes-dir) "002 Calendar.org")
      cjc/org-archive-file (concat (file-name-as-directory cjc/default-org-notes-dir) "003 Archive.org")
      cjc/org-contacts-file (concat (file-name-as-directory cjc/default-org-notes-dir) "004 Contacts.org"))

(setq org-directory cjc/default-org-notes-dir)

(use-package org-contacts
  :ensure nil
  :after org
  :custom (org-contacts-files (list cjc/org-contacts-file)))

(use-package org
  :ensure org-plus-contrib
  :hook (org-mode . cjc/org-mode-setup)
  :config

;; Basic Settings ;;
(setq org-ellipsis " ???"
      org-hide-emphasis-markers t)

(with-eval-after-load 'evil
    (evil-define-key '(normal visual) outline-mode-map (kbd "<tab>") #'org-cycle)
    (evil-define-key '(normal visual) outline-mode-map (kbd "TAB") #'org-cycle))

;; Agenda Settings ;;
(setq org-agenda-start-with-log-mode t
      org-log-done 'time
      org-log-into-drawer t
      org-agenda-files
      (list cjc/org-backlog-file
            cjc/org-calendar-file
            cjc/org-archive-file
            cjc/org-contacts-file)
      org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@)" "|" "DONE(d!)" "CANCELLED(c@)")))

(setq org-refile-targets
  '((cjc/org-archive-file :maxlevel . 1)))

; save Org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)

(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 60)

; configure custom agenda views
(setq org-agenda-custom-commands
 '(("d" "Dashboard"
   ((agenda "" ((org-deadline-warning-days 7)))
    (todo "TODO+STYLE=\"habit\"" ((org-agenda-overriding-header "Habits")))
    (todo "NEXT"
      ((org-agenda-overriding-header "Next Tasks")))
    (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

  ("n" "Next Tasks"
   ((todo "NEXT"
      ((org-agenda-overriding-header "Next Tasks")))))

  ("h" "Home Tasks" tags-todo "+work")
  ("w" "Work Tasks" tags-todo "+home")
  ("m" "Media" tags-todo "+media")))

; default tags
(setq org-tag-alist
  '((:startgroup)
    ("home" . ?h)
    ("work" . ?w)
    ("play" . ?p)
    (:endgroup)
    ("note" . ?n)
    ("media" . ?m)))

(defvar cjc/org-contacts-template "* %(org-contacts-template-name) %^g
:PROPERTIES:
:ADDRESS: %^{ADDRESS}
:BIRTHDAY: %^{BIRTHDAY, yyyy-mm-dd}
:EMAIL: %(org-contacts-template-email)
:NOTE: %^{NOTE}
:END:" "Template for org-contacts.")


  (setq org-capture-templates
    `(("t" "Todo" entry (file+headline ,cjc/org-backlog-file "Tasks")
         "** TODO %?\n  %i\n  %a")
      ("e" "Event" entry (file+headline ,cjc/org-calendar-file "Events")
         "** %?\n %i\n  :SCHEDULED: %^T")
      ("c" "Contact" entry (file cjc/org-contacts-file)
         ,cjc/org-contacts-template)))

;; Load Org Babel Languages ;;
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)))

(push '("conf-unix" . conf-unix) org-src-lang-modes)

;; Automatically Tangle Configuration ;;
  (defun cjc/org-babel-tangle-config ()
    (when (string-equal (buffer-file-name)
                        (expand-file-name "~/dev/dotfiles/Emacs.org"))
      (let ((org-confirm-babel-evaluate nil))
        (org-babel-tangle))))
  
  (add-hook 'org-mode-hook
            (lambda () (add-hook 'after-save-hook
                               #'cjc/org-babel-tangle-config)))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

(defun cjc/org-setup-fonts ()
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
      (set-face-attribute (car face) nil :font "Roboto" :weight 'regular :height (cdr face)))

  ; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(add-hook 'after-change-theme-hook #'cjc/org-setup-fonts t)
(cjc/org-setup-fonts)

(defun cjc/org-toggle-emphasis ()
  "Toggle hiding/showing of org emphasize markers."
  (interactive)
  (if org-hide-emphasis-markers
      (set-variable 'org-hide-emphasis-markers nil)
    (set-variable 'org-hide-emphasis-markers t))
  (org-mode-restart))

(defun cjc/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

)

(defun cjc/org-open-index-file ()
  "Opens the index file from the org notes directory"
  (interactive)
  (find-file cjc/org-index-file))

(cjc/leader-key
  :keymaps 'org-mode-map
  "m" '(:ignore t :which-key "org-mode")
  "me" '(cjc/org-toggle-emphasis :which-key "toggle emphasis"))
  
(cjc/leader-key
  "n" '(:ignore t :which-key "notes")
  "nc" '(org-capture :which-key "capture")
  "nn" '(cjc/org-open-index-file :which-key "open index")
  "na" '(org-agenda :which-key "agenda"))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("???" "???" "???" "???" "???" "???")
        org-superstar-leading-bullet " "))
