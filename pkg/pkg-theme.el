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

  (use-package company-mode
    :defer t
    :config
    (set-face-attribute 'company-template-field
                        nil :background "#373b41" :foreground "#81a2be"))

  (use-package git-gutter
    :defer t
    :config
    (set-face-attribute 'git-gutter:unchanged nil :background "#1d1f21")
    (set-face-attribute 'git-gutter:added nil :foreground "#b5bd68")
    (set-face-attribute 'git-gutter:deleted nil :foreground "#cc6666")
    (set-face-attribute 'git-gutter:modified nil :foreground "#b294bb"))

  (set-face-attribute 'font-lock-doc-face nil :foreground "#969896")
  (set-face-attribute 'fringe nil :background "#1d1f21")
  (set-face-attribute 'vertical-border nil :foreground "#373b41")
  (set-face-attribute 'font-lock-comment-face nil :foreground "#767876"))

(provide 'pkg-theme)
