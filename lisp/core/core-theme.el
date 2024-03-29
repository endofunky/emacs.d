;;; core-theme.el --- Eye candy -*- lexical-binding: t; -*-
(require 'core-lib)
(require 'core-popup)

(use-package doom-themes
  :ensure t
  :demand t
  :commands (doom-themes-org-config)
  :config
  (load-theme 'doom-tokyo-night t)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)

  (custom-set-faces
   `(flymake-error ((t :foreground "#f7768e" :underline nil)))
   `(flymake-note ((t :foreground "#f7768e" :underline nil)))
   `(flymake-warning ((t :foreground "#f7768e" :underline nil)))
   `(font-lock-builtin-face ((t :foreground "#73daca")))
   `(font-lock-type-face ((t :foreground "#73daca")))
   `(hl-line ((t :background "#24283b")))
   `(nix-attribute-face ((t :foreground "#7dcfff")))
   `(markdown-code-face ((t :background "#1a1b26")))
   `(org-block ((t :background "#1a1b26")))
   `(org-block-begin-line ((t :inherit nil :foreground "#343952" :background "#1a1b26" :extend t)))
   `(org-block-end-line ((t :inherit nil :foreground "#343952" :background "#1a1b26" :extend t)))
   `(org-block-background ((t :background "#1a1b26")))
   `(poe-popup-dimmed-face ((t :background "#12121a")))
   `(vertico-current ((t :background "#24283b"))))

  (with-eval-after-load 'org
    (set-face-attribute 'org-block-begin-line nil
                        :box '(:line-width (-1 . 10)
                               :color "#1a1b26"
                               :style nil))

    (set-face-attribute 'org-block-end-line nil
                        :box '(:line-width (-1 . 10)
                               :color "#1a1b26"
                               :style nil))))

(use-package doom-nano-modeline
  :straight (doom-nano-modeline :type git
                                :host github
                                :repo "ronisbr/doom-nano-modeline"
                                :branch "main"
                                :files ("*.el"))
  :custom
  (doom-nano-modeline-position 'bottom)
  :config
  (doom-nano-modeline-mode t))

(use-package hl-todo
  :commands (hl-todo-mode)
  :custom
  (hl-todo-keyword-faces '(("HOLD"   . "#f7768e")
                           ("TODO"   . "#f7768e")
                           ("FAIL"   . "#f7768e")
                           ("NOTE"   . "#ff9e64")
                           ("HACK"   . "#ff9e64")
                           ("FIXME"  . "#f7768e")
                           ("XXX"    . "#f7768e")))
  :hook
  (prog-mode . hl-todo-mode))

(provide 'core-theme)
