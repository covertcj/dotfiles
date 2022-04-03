(setq org-directory cjc/org-directory
      org-roam-directory cjc/org-directory)

(use-package! org-modern
  :hook (org-mode . org-modern-mode))
