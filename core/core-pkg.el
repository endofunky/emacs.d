(require 'core-lib)

(use-package abbrev
  :diminish abbrev-mode
  :defer t)

(use-package autorevert
  :custom
  (auto-revert-interval 1)
  (auto-revert-verbose nil)
  (global-auto-revert-non-file-buffers t)
  :config
  (global-auto-revert-mode 1))

(use-package comint
  :defer t
  :config
  (setq comint-scroll-to-bottom-on-output 'others)
  (ef-add-hook comint-mode-hook
    (setq truncate-lines nil)
    (set (make-local-variable 'truncate-partial-width-windows) nil)))

(use-package compile
  :defer t
  :hook (compilation-mode . ef-compilation-mode-hook)
  :custom
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  (compilation-message-face 'default)
  (compilation-scroll-output 'first-error)
  :config
  (defun ef-compilation-exit-autoclose (buffer msg)
    (when (string-match-p (regexp-quote "finished") msg)
      (bury-buffer)
      (delete-window (get-buffer-window (get-buffer "*compilation*")))))

  (add-to-list 'compilation-finish-functions #'ef-compilation-exit-autoclose)

  (defun ef-compilation-mode-hook ()
    (setq-local bidi-display-reordering nil)
    (visual-line-mode t)))

(use-package fringe
  :config
  (set-fringe-mode 0))

(use-package hl-line
  :config
  (global-hl-line-mode 1))

(use-package hideshow
  :commands hs-minor-mode
  :diminish hs-minor-mode
  :hook (prog-mode . hs-minor-mode))

(use-package minibuffer
  :config
  (ef-add-hook minibuffer-setup-hook
    (setq gc-cons-threshold most-positive-fixnum))

  (ef-add-hook minibuffer-exit-hook
    (setq gc-cons-threshold ef-initial-gc-cons-threshold)))

(use-package paren
  :config
  (setq show-paren-delay 0)
  (show-paren-mode t))

(use-package prog-mode
  :config
  (global-prettify-symbols-mode -1))

(use-package savehist
  :defer 1
  :config
  (savehist-mode t)
  (setq savehist-file (expand-file-name "savehist" user-emacs-directory)
        savehist-additional-variables '(search ring regexp-search-ring)
        savehist-autosave-interval 60
        history-length 1000))

(use-package saveplace
  :config
  (require 'saveplace)
  (setq save-place-file
        (expand-file-name "saveplace" user-emacs-directory))
  (setq-default save-place t))

(use-package smerge-mode
  :commands smerge-mode
  :init
  (setq smerge-command-prefix (kbd "C-s"))

  (ef-add-hook (find-file-hook after-revert-hook) :fn ef-enable-smerge-maybe
    "Auto-enable `smerge-mode' when merge conflict is detected."
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil :noerror)
        (smerge-mode t)))))

(use-package term
  :config
  (ef-add-hook term-mode-hook
    (setq ansi-term-color-vector
          [term
           term-color-black
           term-color-red
           term-color-green
           term-color-yellow
           term-color-blue
           term-color-magenta
           term-color-cyan
           term-color-white])))

(use-package whitespace
  :diminish (whitespace-mode global-whitespace-mode)
  :config
  (global-whitespace-mode 1)
  (setq-default show-trailing-whitespace nil)
  (setq whitespace-style (quote (face trailing))))

(provide 'core-pkg)
