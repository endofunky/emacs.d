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

  (use-package git-gutter
    :defer t
    :config
    (set-face-attribute 'git-gutter:unchanged nil :background "#1d1f21")
    (set-face-attribute 'git-gutter:added nil :foreground "#b5bd68")
    (set-face-attribute 'git-gutter:deleted nil :foreground "#cc6666")
    (set-face-attribute 'git-gutter:modified nil :foreground "#b294bb"))

  (use-package diff-hl
    :defer t
    :config
    (set-face-attribute 'diff-hl-change nil :foreground "#de935f"))

  (use-package popup
    :defer t
    :config
    (set-face-attribute 'popup-summary-face nil :background "#FF0000")
    (set-face-attribute 'popup-scroll-bar-background-face nil :background "#ffffff")
    (set-face-attribute 'popup-scroll-bar-foreground-face nil :background "#b4b7b4")
    (set-face-attribute 'popup-face nil :background "#282a2e" :foreground "#c5c8c6")
    (set-face-attribute 'popup-menu-selection-face nil :background "#373b41" :foreground "#81a2be"))

  (set-face-attribute 'font-lock-doc-face nil :foreground "#969896")
  (set-face-attribute 'fringe nil :background "#1d1f21")
  (set-face-attribute 'vertical-border nil :foreground "#000000")
  (set-face-attribute 'font-lock-comment-face nil :foreground "#767876"))

(provide 'pkg-theme)
