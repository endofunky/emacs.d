(use-package abbrev
  :diminish abbrev-mode
  :defer t)

(use-package autorevert
  :config
  (global-auto-revert-mode 1)
  (setq auto-revert-verbose nil
        global-auto-revert-non-file-buffers t))

(use-package comint
  :defer t
  :config
  (setq comint-scroll-to-bottom-on-output 'others)
  (defun ef-comint-mode-hook ()
    (setq truncate-lines nil)
    (set (make-local-variable 'truncate-partial-width-windows) nil))
  (add-hook 'comint-mode-hook 'ef-comint-mode-hook))

(use-package compile
  :defer t
  :config
  (setq compilation-always-kill t
        compilation-message-face 'default)

  (defun ef-compilation-exit-autoclose (status code msg)
    (when (and (eq status 'exit) (zerop code))
      (bury-buffer)
      (delete-window (get-buffer-window (get-buffer "*compilation*"))))
    (cons msg code))

  (setq compilation-exit-message-function 'ef-compilation-exit-autoclose))

(use-package fringe
  :config
  (set-fringe-mode 0))

(use-package hl-line
  :config
  (global-hl-line-mode 1))

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
  :ensure t
  :commands smerge-mode
  :init
  (setq smerge-command-prefix (kbd "C-s"))

  (defun ef-enable-smerge-maybe ()
    "Auto-enable `smerge-mode' when merge conflict is detected."
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil :noerror)
        (smerge-mode t))))

  (add-hook 'find-file-hook 'ef-enable-smerge-maybe t)
  (add-hook 'after-revert-hook 'sm-try-smerge t))

(use-package whitespace
  :diminish (whitespace-mode global-whitespace-mode)
  :config
  (global-whitespace-mode 1)
  (setq-default show-trailing-whitespace nil)
  (setq whitespace-style (quote (face trailing))))

(provide 'core-pkg)
