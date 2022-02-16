(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

;; Font Settings
(setq cjc/default-font-mono "Roboto Mono")
(setq cjc/default-font-variable "Roboto")
(setq cjc/default-font-height 150)

;; Theme Settings
(setq cjc/default-dark-theme 'doom-challenger-deep)
(setq cjc/default-light-theme 'doom-tomorrow-day)
(setq cjc/theme-list
  (list cjc/default-dark-theme
	cjc/default-light-theme))

;; Org Settings
(setq cjc/default-org-notes-dir (expand-file-name "~/org/"))

(setq inhibit-startup-message t)

; remove menu's/scroll bars
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

; pad the edge of the window
(set-fringe-mode 10)

; no annoying bells
(setq visible-bell nil)

; keep the cursor away from the edge, don't jump when scrolling
(setq scroll-margin 8)
(setq scroll-conservatively 1)

; start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

; line numbers
(global-display-line-numbers-mode t)
(setq display-line-numbers 'relative)

; display cursor's column in the status bar
(column-number-mode)

; hide line numbers in certain modes
(dolist (mode '(term-mode-hook
		eshell-mode-hook))
   (add-hook mode (lambda () (display-line-numbers-mode 0))))

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

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
		      (expand-file-name "~/.config/emacs/config.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

;; Fonts
(set-face-attribute 'default nil :font cjc/default-font-mono :height cjc/default-font-height)
(set-face-attribute 'fixed-pitch nil :font cjc/default-font-mono :height cjc/default-font-height)
(set-face-attribute 'variable-pitch nil :font cjc/default-font-variable :height cjc/default-font-height)

(defvar after-change-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme' or `enable-theme'.")

(defadvice load-theme (after run-after-change-theme-hook activate)
  "Run `after-change-theme-hook'."
  (run-hooks 'after-change-theme-hook))

(defadvice enable-theme (after run-after-change-theme-hook activate)
  "Run `after-change-theme-hook'."
  (run-hooks 'after-change-theme-hook))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold nil
      doom-themes-enable-italic nil)
  (load-theme (car cjc/theme-list) t)
  (doom-themes-org-config))

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

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

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

(use-package vertico
  :bind
  (:map vertico-map
   ("C-j" . vertico-next)
   ("C-k" . vertico-previous)
   ("C-f" . vertico-exit))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package savehist
  :config
  (savehist-mode))

(use-package marginalia
  :after vertico
  :config
  (marginalia-mode))
