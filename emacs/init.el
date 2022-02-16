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
(setq cjc/default-font-mono "Iosevka Term SS12")
(setq cjc/default-font-variable "Iosevka Term SS12")
(setq cjc/default-font-height 150)

;; Theme Settings
(setq cjc/default-dark-theme 'doom-1337)
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

(use-package general
  :config
  (general-evil-setup)
  (general-create-definer cjc/leader-key
    :states '(normal visual emacs motion)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "C-SPC")

(cjc/leader-key
  "b"  '(:ignore t :which-key "buffers")
  "bb" '(consult-buffer :which-key "switch buffer")
  "bk" '(kill-current-buffer :which-key "kill current buffer")
  "bK" '(kill-buffer :which-key "kill target buffer"))

(cjc/leader-key
  "t"  '(:ignore t :which-key "settings")
  "tf" '(text-scale-adjust :which-key "font scale")
  "tF" '(text-scale-mode :which-key "font scale toggle")
  "tt" '(cjc/toggle-themes :which-key "theme cycle")
  "tT" '(consult-theme :which-key "theme select"))

)

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

; TODO clean up these default settings
(use-package consult
    ;; Replace bindings. Lazily loaded due by `use-package'.
    :bind (;; C-c bindings (mode-specific-map)
	   ("C-c h" . consult-history)
	   ("C-c m" . consult-mode-command)
	   ("C-c k" . consult-kmacro)
	   ;; C-x bindings (ctl-x-map)
	   ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
	   ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
	   ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
	   ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
	   ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
	   ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
	   ;; Custom M-# bindings for fast register access
	   ("M-#" . consult-register-load)
	   ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
	   ("C-M-#" . consult-register)
	   ;; Other custom bindings
	   ("M-y" . consult-yank-pop)                ;; orig. yank-pop
	   ("<help> a" . consult-apropos)            ;; orig. apropos-command
	   ;; M-g bindings (goto-map)
	   ("M-g e" . consult-compile-error)
	   ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
	   ("M-g g" . consult-goto-line)             ;; orig. goto-line
	   ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
	   ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
	   ("M-g m" . consult-mark)
	   ("M-g k" . consult-global-mark)
	   ("M-g i" . consult-imenu)
	   ("M-g I" . consult-imenu-multi)
	   ;; M-s bindings (search-map)
	   ("M-s d" . consult-find)
	   ("M-s D" . consult-locate)
	   ("M-s g" . consult-ripgrep)
	   ("M-s G" . consult-git-grep)
	   ("M-s r" . consult-ripgrep)
	   ("M-s l" . consult-line)
	   ("M-s L" . consult-line-multi)
	   ("M-s m" . consult-multi-occur)
	   ("M-s k" . consult-keep-lines)
	   ("M-s u" . consult-focus-lines)
	   ;; Isearch integration
	   ("M-s e" . consult-isearch-history)
	   :map isearch-mode-map
	   ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
	   ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
	   ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
	   ("M-s L" . consult-line-multi))           ;; needed by consult-line to detect isearch

    ;; Enable automatic preview at point in the *Completions* buffer. This is
    ;; relevant when you use the default completion UI. You may want to also
    ;; enable `consult-preview-at-point-mode` in Embark Collect buffers.
    :hook (completion-list-mode . consult-preview-at-point-mode)

    ;; The :init configuration is always executed (Not lazy)
    :init

    ;; Optionally configure the register formatting. This improves the register
    ;; preview for `consult-register', `consult-register-load',
    ;; `consult-register-store' and the Emacs built-ins.
    (setq register-preview-delay 0
	  register-preview-function #'consult-register-format)

    ;; Optionally tweak the register preview window.
    ;; This adds thin lines, sorting and hides the mode line of the window.
    (advice-add #'register-preview :override #'consult-register-window)

    ;; Optionally replace `completing-read-multiple' with an enhanced version.
    (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

    ;; Use Consult to select xref locations with preview
    (setq xref-show-xrefs-function #'consult-xref
	  xref-show-definitions-function #'consult-xref)

    ;; Configure other variables and modes in the :config section,
    ;; after lazily loading the package.
    :config

    ;; Optionally configure preview. The default value
    ;; is 'any, such that any key triggers the preview.
    ;; (setq consult-preview-key 'any)
    ;; (setq consult-preview-key (kbd "M-."))
    ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
    ;; For some commands and buffer sources it is useful to configure the
    ;; :preview-key on a per-command basis using the `consult-customize' macro.
    (consult-customize
     consult-theme
     :preview-key '(:debounce 0.2 any)
     consult-ripgrep consult-git-grep consult-grep
     consult-bookmark consult-recent-file consult-xref
     consult--source-bookmark consult--source-recent-file
     consult--source-project-recent-file
     :preview-key (kbd "M-."))

    ;; Optionally configure the narrowing key.
    ;; Both < and C-+ work reasonably well.
    (setq consult-narrow-key "<") ;; (kbd "C-+")

    ;; Optionally make narrowing help available in the minibuffer.
    ;; You may want to use `embark-prefix-help-command' or which-key instead.
    ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

    ;; By default `consult-project-root-function' uses `project-root' from project.el.
    ;; Optionally configure a different project root function.
    ;; There are multiple reasonable alternatives to chose from.
    ;;;; 1. project.el (the default)
    ;; (setq consult-project-root-function #'consult--project-root-default-function)
    ;;;; 2. projectile.el (projectile-project-root)
    ;; (autoload 'projectile-project-root "projectile")
    ;; (setq consult-project-root-function #'projectile-project-root)
    ;;;; 3. vc.el (vc-root-dir)
    ;; (setq consult-project-root-function #'vc-root-dir)
    ;;;; 4. locate-dominating-file
    ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
  )

(use-package magit
  :after general
  :config
  (cjc/leader-key
   "g"  '(:ignore t :which-key "git")
   "gg" '(magit-status :which-key "status")
   "gb" '(magit-branch :which-key "branch")
   "gc" '(magit-branch-or-checkout :which-key "branch or checkout")))

; TODO this has worked in the past, but is untested here
(when (eq system-type 'windows-nt)
  (setenv "SSH_ASKPASS" "git-gui--askpass")
  (use-package ssh-agency))
