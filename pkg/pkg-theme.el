(use-package base16-theme
  :ensure t
  :config
  (setq custom-safe-themes t)
  (load-theme 'base16-tomorrow-night t)

  (defun ts/theme-add-watchwords ()
    "Highlight FIXME, TODO, and NOCOMMIT in code"
    (font-lock-add-keywords
     nil '(("\\<\\(FIXME\\|BUG\\|XXX\\|TODO\\|NOCOMMIT\\)\\>"
            1 '((:foreground "#cc6666") (:weight bold)) t))))

  (add-hook 'prog-mode-hook 'ts/theme-add-watchwords)

  (set-face-attribute 'company-template-field nil :background "#373b41" :foreground "#81a2be")
  (set-face-attribute 'vertical-border nil :foreground "#373b41")
  (set-face-attribute 'font-lock-comment-face nil :foreground "#767876"))

(provide 'pkg-theme)
