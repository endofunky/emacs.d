(use-package base16-theme
  :ensure t
  :config
  (setq custom-safe-themes t)

  (setq ef-base16-enabled-theme 'base16-tomorrow-night)
  (setq ef-base16-enabled-theme-colors 'base16-tomorrow-night-colors)

  (load-theme ef-base16-enabled-theme t)

  (defun ef-color (name)
    "Returns the base color for `name'"
    (plist-get base16-tomorrow-night-colors name))

  (defun ef-theme-add-watchwords ()
    "Highlight FIXME, TODO, and NOCOMMIT in code"
    (font-lock-add-keywords
     nil '(("\\<\\(FIXME\\|BUG\\|XXX\\|TODO\\|NOCOMMIT\\)\\>"
            1 '((:foreground "#cc6666") (:weight bold)) t))))

  (add-hook 'prog-mode-hook 'ef-theme-add-watchwords)

  (set-face-attribute 'org-document-title nil :height 1.0 :weight 'medium)
  (set-face-attribute 'org-block nil :foreground (ef-color :base04))
  (set-face-attribute 'font-lock-doc-face nil :foreground (ef-color :base03))
  (set-face-attribute 'fringe nil :background (ef-color :base00))
  (set-face-attribute 'vertical-border nil :foreground (ef-color :base01)))

(provide 'pkg-theme)
