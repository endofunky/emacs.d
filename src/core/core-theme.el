(require 'core-lib)
(require 'core-shackle)

(use-package base16-theme
  :ensure t
  :custom
  (custom-safe-themes t)
  (base16-theme-256-color-source 'base16-shell)
  :config
  (require 'base16-tomorrow-night-theme)
  (defvar ef-base16-enabled-theme 'base16-tomorrow-night)
  (defvar ef-base16-enabled-theme-colors 'base16-tomorrow-night-theme-colors)

  (load-theme ef-base16-enabled-theme t)

  (defun ef-color (name)
    "Returns the base color for `name'"
    (plist-get base16-tomorrow-night-theme-colors name))

  (ef-add-hook window-configuration-change-hook :fn ef-dim-popups
    (walk-windows (lambda (w)
                    (with-current-buffer (window-buffer w)
                      (if (ef-popup--buffer-p (window-buffer w))
                          (buffer-face-set '(:background "#151617"))
                        (buffer-face-set 'default))))))

  (ef-add-hook prog-mode-hook :fn ef-theme-add-watchwords
    "Highlight FIXME, TODO, and NOCOMMIT in code"
    (font-lock-add-keywords
     nil '(("\\<\\(FIXME\\|BUG\\|XXX\\|TODO\\|NOCOMMIT\\)\\>"
            1 '((:foreground "#cc6666") (:weight bold)) t))))

  (set-face-attribute 'line-number nil :background (ef-color :base00) :foreground (ef-color :base02))
  (set-face-attribute 'line-number-current-line nil :background (ef-color :base01))
  (set-face-attribute 'font-lock-doc-face nil :foreground (ef-color :base03))
  (set-face-attribute 'fringe nil :background (ef-color :base00))
  (set-face-attribute 'header-line nil :foreground (ef-color :base0E) :background (ef-color :base00))
  (set-face-attribute 'trailing-whitespace nil :foreground nil :background (ef-color :base08))
  (set-face-attribute 'vertical-border nil :foreground (ef-color :base01)))

(use-package pdf-tools
  :defer t
  :custom
  (pdf-view-midnight-colors `(,(ef-color :base05) . ,(ef-color :base00))))

(use-package macrostep
  :defer t
  :config
  (set-face-attribute 'macrostep-expansion-highlight-face nil :background (ef-color :base01) :extend t))

(use-package ivy
  :defer t
  :config
  (setcdr (assoc t ivy-format-functions-alist) #'ivy-format-function-line)
  (set-face-attribute 'ivy-current-match nil :foreground (ef-color :base09) :background (ef-color :base01) :extend t))

(use-package hl-line
  :defer t
  :config
  (set-face-attribute 'hl-line nil :background (ef-color :base01) :extend t))

(use-package flycheck
  :defer t
  :config
  (set-face-attribute 'flycheck-error nil :underline `(:color ,(ef-color :base08) :style line))
  (set-face-attribute 'flycheck-info nil :underline `(:color ,(ef-color :base0B) :style line))
  (set-face-attribute 'flycheck-warning nil :underline `(:color ,(ef-color :base09) :style line)))

(use-package org
  :defer t
  :config
  (set-face-attribute 'org-document-title nil :height 1.0 :weight 'medium)
  (set-face-attribute 'org-block nil :foreground (ef-color :base04)))

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

(provide 'core-theme)
