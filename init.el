;; TODO come back and change these
(set-face-attribute 'default nil
		    :font "Roboto Mono"
		    :height 160)

(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(setq scroll-margin 8)
(setq scroll-conservatively 1)

(setq visible-bell nil)

(column-number-mode)
(global-display-line-numbers-mode t)
; disable line numbers for certain modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

; make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Package Management with use-package
(require 'package)

; setup the package archives and make sure there is a local copy
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

; make sure use-package is available
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Completion
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
  :after (ivy)
  :init
  (ivy-rich-mode 1))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)))

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold nil
	doom-themes-enable-italic nil)
  ;(load-theme 'doom-challenger-deep t)
  (load-theme 'doom-palenight t)
  (doom-themes-org-config))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))

(use-package general
  :config
  (general-evil-setup)
  (general-create-definer cjc/leader-key
			  :keymaps '(normal insert visual emacs)
			  :prefix "SPC"
			  :non-normal-prefix "C-SPC")

  (cjc/leader-key
   "b"  '(:ignore b :which-key "buffers")
   "bb" '(counsel-switch-buffer :which-key "switch buffers")))

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
  :after (evil)
  :config
  (evil-collection-init))

(use-package hydra
  :after (general)
  :config
  ;; TODO: probably move this somewhere else that ends up depending on hydra
  (defhydra hydra-scale-text (:timeout 6)
    "scale editor text"
    ("j" text-scale-increase "in")
    ("k" text-scale-decrease "out")
    ("f" nil "finished" :exit t))

  ;; TODO: probably move this somewhere else that ends up depending on hydra
  (cjc/leader-key
    "t" '(:ignore t :which-key "toggle settings")
    "tf" '(hydra-scale-text/body :which-key "font scaling")))

(use-package projectile
  :diminish projectile-mode
  :init
  (setq projectile-project-search-path '())
  (when (file-directory-p "~/dev") (push "~/dev" projectile-project-search-path))
  (when (file-directory-p "~/work") (push "~/work" projectile-project-search-path))

  :config
  (projectile-mode 1)
  (cjc/leader-key
    "p" '(:keymap projectile-command-map :wk "projectile")))
  
(use-package magit)
  ;:custom
  ;(magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


; (use-package helpful


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("47db50ff66e35d3a440485357fb6acb767c100e135ccdf459060407f8baea7b2" "cbdf8c2e1b2b5c15b34ddb5063f1b21514c7169ff20e081d39cf57ffee89bc1e" default))
 '(ivy-mode t)
 '(package-selected-packages
   '(evil-magit magit-evil magit projectile hydra evil-collection general doom-themes ivy-rich counsel which-key use-package swiper rainbow-delimiters doom-modeline)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
