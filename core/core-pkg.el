(use-package abbrev
  :diminish abbrev-mode
  :defer t)

(use-package autorevert
  :custom
  (auto-revert-verbose nil)
  (global-auto-revert-non-file-buffers t)
  :config
  (global-auto-revert-mode 1))

(use-package comint
  :defer t
  :custom
  (comint-scroll-to-bottom-on-output 'others)
  :config
  (defun ef-comint-mode-hook ()
    (setq-local truncate-lines nil)
    (set (make-local-variable 'truncate-partial-width-windows) nil))
  (add-hook 'comint-mode-hook 'ef-comint-mode-hook))

(use-package compile
  :defer t
  :custom
  (compilation-always-kill t)
  (compilation-message-face 'default)
  (compilation-exit-message-function 'ef-compilation-exit-autoclose)
  :config
  (defun ef-compilation-exit-autoclose (status code msg)
    (when (and (eq status 'exit) (zerop code))
      (bury-buffer)
      (delete-window (get-buffer-window (get-buffer "*compilation*"))))
    (cons msg code)))

(use-package fringe
  :config
  (set-fringe-mode 0))

(use-package hl-line
  :config
  (global-hl-line-mode 1))

(use-package minibuffer
  :config
  (defun ef-minibuffer-setup-hook ()
    (setq-local gc-cons-threshold most-positive-fixnum))

  (defun ef-minibuffer-exit-hook ()
    (setq-local gc-cons-threshold ef-initial-gc-cons-threshold))

  (add-hook 'minibuffer-setup-hook #'ef-minibuffer-setup-hook)
  (add-hook 'minibuffer-exit-hook #'ef-minibuffer-exit-hook))

(use-package paren
  :custom
  (show-paren-delay 0)
  :config
  (show-paren-mode t))

(use-package prog-mode
  :config
  (global-prettify-symbols-mode -1))

(use-package savehist
  :defer 1
  :custom
  (savehist-file (expand-file-name "savehist" user-emacs-directory))
  (savehist-additional-variables '(search ring regexp-search-ring))
  (savehist-autosave-interval 60)
  (history-length 1000)
  :config
  (savehist-mode t))

(use-package saveplace
  :custom
  (save-place-file (expand-file-name "saveplace" user-emacs-directory))
  (save-place t)
  :config
  (require 'saveplace))

(use-package smerge-mode
  :ensure t
  :commands smerge-mode
  :custom
  (smerge-command-prefix (kbd "C-s"))
  :init

  (defun ef-enable-smerge-maybe ()
    "Auto-enable `smerge-mode' when merge conflict is detected."
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil :noerror)
        (smerge-mode t))))

  (add-hook 'find-file-hook 'ef-enable-smerge-maybe t)
  (add-hook 'after-revert-hook 'ef-enable-smerge-maybe t))

(use-package term
  :config
  (defun ef-term-mode-hook ()
    (setq-local ansi-term-color-vector
                [term
                 term-color-black
                 term-color-red
                 term-color-green
                 term-color-yellow
                 term-color-blue
                 term-color-magenta
                 term-color-cyan
                 term-color-white]))

  (add-hook 'term-mode-hook 'ef-term-mode-hook))

(use-package whitespace
  :diminish (whitespace-mode global-whitespace-mode)
  :custom
  (show-trailing-whitespace nil)
  (whitespace-style (quote (face trailing)))
  :config
  (global-whitespace-mode 1))

(provide 'core-pkg)
