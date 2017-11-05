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

(use-package smerge-mode
  :defer t
  :config
  (set-face-attribute 'smerge-refined-added nil :background nil :foreground (ef-color :base0B))
  (set-face-attribute 'smerge-refined-removed nil :background nil :foreground (ef-color :base08)))

(use-package magit
  :defer t
  :config
  (set-face-attribute 'magit-section-heading nil :background nil :foreground (ef-color :base0A) :weight 'medium)
  (set-face-attribute 'magit-branch-local nil :background nil :foreground (ef-color :base0C))
  (set-face-attribute 'magit-branch-remote nil :background nil :foreground (ef-color :base0B))
  (set-face-attribute 'magit-hash nil :background nil :foreground (ef-color :base03))
  (set-face-attribute 'magit-diff-file-heading nil :background nil :foreground (ef-color :base05))
  (set-face-attribute 'magit-diff-added nil :background nil :foreground (ef-color :base0B))
  (set-face-attribute 'magit-diff-added-highlight nil :background (ef-color :base01) :foreground (ef-color :base0B))
  (set-face-attribute 'magit-diff-removed nil :background nil :foreground (ef-color :base08))
  (set-face-attribute 'magit-diff-removed-highlight nil :background (ef-color :base01) :foreground (ef-color :base08))
  (set-face-attribute 'magit-diff-context-highlight nil :background (ef-color :base01)))

(use-package tuareg
  :defer t
  :config
  (set-face-attribute 'tuareg-font-double-colon-face nil :foreground (ef-color :base04)))

(provide 'pkg-theme)
