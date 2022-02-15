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

(use-package evil
  :config 
  (evil-mode 1))
