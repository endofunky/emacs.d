(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (setq custom-safe-themes t)

  (color-theme-sanityinc-tomorrow-night)

  (defun ts/theme-add-watchwords ()
    "Highlight FIXME, TODO, and NOCOMMIT in code"
    (font-lock-add-keywords
     nil '(("\\<\\(FIXME\\|BUG\\|XXX\\|TODO\\|NOCOMMIT\\)\\>"
            1 '((:foreground "#cc6666") (:weight bold)) t))))

  (add-hook 'prog-mode-hook 'ts/theme-add-watchwords)

  ;; Don't show a border on the modeline
  (set-face-attribute 'mode-line nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :box nil))

(provide 'pkg-tomorrow-theme)
