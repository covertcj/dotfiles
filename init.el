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
(setq cjc/default-font-height 160)
(set-face-attribute 'default nil :font "Roboto Mono" :height cjc/default-font-height)
(set-face-attribute 'fixed-pitch nil :font "Roboto Mono" :height cjc/default-font-height)
(set-face-attribute 'variable-pitch nil :font "Roboto" :height cjc/default-font-height)

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

;; Basic Keybindings ;;
; let ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general
  :config
  (general-evil-setup)
  (general-create-definer cjc/leader-key
    :states 'normal
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
 "b"  '(:ignore b :which-key "buffers")
 "bb" '(counsel-switch-buffer :which-key "switch buffers"))

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
      (error (load-theme next-theme t)))
    (cjc/org-setup-fonts)))

(cjc/leader-key
  "t" '(:ignore t :which-key "toggle settings")
  "tf" '(hydra-scale-text/body :which-key "font scaling")
  "tt" '(cjc/toggle-themes :which-key "theme")
  "tT" '(counsel-load-theme :which-key "theme"))

))

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1))

;(setq cjc/default-dark-theme 'doom-palenight)
(setq cjc/default-dark-theme 'doom-challenger-deep)
(setq cjc/default-light-theme 'doom-solarized-light)
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

(use-package ivy
  :diminish
  :bind (:map ivy-minibuffer-map
	      ("TAB" . ivy-alt-done)
	      ("C-j" . ivy-next-line)
	      ("C-k" . ivy-previous-line))
  :config
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

(use-package forge)
(use-package magit
  :config
  (cjc/leader-key
   "g"  '(:ignore g :which-key "git")
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

(use-package org
  :hook (org-mode . cjc/org-mode-setup)
  :config

;; Basic Settings ;;
(setq org-ellipsis " ▾"
      org-hide-emphasis-markers t)

;; Agenda Settings ;;
(setq org-agenda-start-with-log-mode t
      org-log-done 'time
      org-log-into-drawer t
      org-agenda-files
      '("~/Google Drive/Org Notes/Tasks.org"
        "~/Google Drive/Org Notes/Birthdays.org")
      org-todo-keywords
      '(("TODO(t)" "NEXT(n)" "|" "DONE(d!)" "CANCELLED(C@)")))

(setq org-refile-targets
  '(("Archive.org" :maxlevel . 1)
    ("Tasks.org" :maxlevel . 1)))

; save Org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)

(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 60)

; configure custom agenda views
(setq org-agenda-custom-commands
 '(("d" "Dashboard"
   ((agenda "" ((org-deadline-warning-days 7)))
    (todo "NEXT"
      ((org-agenda-overriding-header "Next Tasks")))
    (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

  ("n" "Next Tasks"
   ((todo "NEXT"
      ((org-agenda-overriding-header "Next Tasks")))))

  ("h" "Home Tasks" tags-todo "+work")
  ("w" "Work Tasks" tags-todo "+home")
  ("m" "Work Tasks" tags-todo "+media")))

; default tags
(setq org-tag-alist
  '((:startgroup)
    (:endgroup)
    ("home" . ?H)
    ("work" . ?W)
    ("note" . ?N)
    ("media" . ?M)))

;; Load Org Babel Languages ;;
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)))

(push '("conf-unix" . conf-unix) org-src-lang-modes)

;; Automatically Tangle Configuration ;;
  (defun cjc/org-babel-tangle-config ()
    (when (string-equal (buffer-file-name)
                        ; TODO: rename this when the config is ready
                        (expand-file-name "~/.emacs.d/Emacs.org"))
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

(cjc/leader-key
  :keymaps 'org-mode-map
  "m" '(:ignore t :which-key "org-mode")
  "me" '(cjc/org-toggle-emphasis :which-key "toggle emphasis"))

)

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("❖" "✱" "✹" "✸" "✦" "✧")
	org-superstar-leading-bullet " "))
