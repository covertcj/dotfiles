;(setq doom-ir-black-brighter-comments t)
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
      "t t" #'cjc/toggle-themes
      "t T" #'load-theme)
