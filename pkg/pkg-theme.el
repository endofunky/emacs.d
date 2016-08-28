(use-package base16-theme
  :ensure t
  :config
  (setq custom-safe-themes t)

  (setq ts/base16-enabled-theme 'base16-tomorrow-night)
  (setq ts/base16-enabled-theme-colors 'base16-tomorrow-night-colors)

  (load-theme ts/base16-enabled-theme t)

  (defun ts/color (name)
    "Returns the base color for `name'"
    (plist-get base16-tomorrow-night-colors name))

  (defun ts/theme-add-watchwords ()
    "Highlight FIXME, TODO, and NOCOMMIT in code"
    (font-lock-add-keywords
     nil '(("\\<\\(FIXME\\|BUG\\|XXX\\|TODO\\|NOCOMMIT\\)\\>"
            1 '((:foreground "#cc6666") (:weight bold)) t))))

  (add-hook 'prog-mode-hook 'ts/theme-add-watchwords)

  (use-package diff-hl
    :defer t
    :config
    (set-face-attribute 'diff-hl-change nil :foreground (ts/color :base09)))

  (use-package popup
    :defer t
    :config
    (set-face-attribute 'popup-tip-face nil :background (ts/color :base01) :foreground (ts/color :base04))
    (set-face-attribute 'popup-scroll-bar-background-face nil :background (ts/color :base07))
    (set-face-attribute 'popup-scroll-bar-foreground-face nil :background (ts/color :base04))
    (set-face-attribute 'popup-face nil :background (ts/color :base01) :foreground (ts/color :base05))
    (set-face-attribute 'popup-menu-selection-face nil :background (ts/color :base02) :foreground (ts/color :base0D)))

  (set-face-attribute 'font-lock-doc-face nil :foreground (ts/color :base03))
  (set-face-attribute 'fringe nil :background (ts/color :base00))
  (set-face-attribute 'vertical-border nil :foreground (ts/color :base01)))

(provide 'pkg-theme)
