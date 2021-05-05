(require 'core-lib)

(use-package abbrev
  :defer t)

(use-package auth-source
  :ensure t
  :commands (ef-auth-user ef-auth-password auth-source-search)
  :config
  (defun ef-auth-password (host)
    (funcall (plist-get (car (auth-source-search :host host)) :secret)))

  (defun ef-auth-user (host)
    (plist-get (car (auth-source-search :host host)) :user)))

(use-package autorevert
  :custom
  (auto-revert-interval 1)
  (auto-revert-verbose nil)
  (global-auto-revert-non-file-buffers t)
  :config
  (global-auto-revert-mode 1))

(use-package comint
  :defer t
  :custom
  (comint-move-point-for-output 'others)
  :functions (ef-comint-mode-hook)
  :config
  (ef-add-hook comint-mode-hook
    (setq truncate-lines nil)
    (set (make-local-variable 'truncate-partial-width-windows) nil)))

(use-package compile
  :defer t
  :custom
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  (compilation-message-face 'default)
  (compilation-scroll-output 'first-error)
  :functions (ef-compilation-mode-hook
	      ef-compilation-exit-autoclose)
  :config
  (defun ef-compilation-exit-autoclose (buffer msg)
    (when (string-match-p (regexp-quote "finished") msg)
      (if (> (length (window-list)) 1)
          (delete-window (get-buffer-window buffer))
        (bury-buffer buffer))))

  (add-to-list 'compilation-finish-functions #'ef-compilation-exit-autoclose)

  (ef-add-hook compilation-mode-hook
    (setq-local bidi-display-reordering nil)
    (visual-line-mode t)))

(use-package files
  :config
  (setq save-silently t))

(use-package fringe
  :config
  (set-fringe-mode 0))

(use-package hl-line
  :config
  (global-hl-line-mode 1))

(use-package hideshow
  :commands hs-minor-mode
  :hook (prog-mode . hs-minor-mode))

(use-package flyspell
  :hook (text-mode . flyspell-mode))

(use-package ispell
  :hook (text-mode . ispell-minor-mode))

(use-package minibuffer
  :custom
  (enable-recursive-minibuffers t)
  :functions (ef-minibuffer-setup-hook
	      ef-minibuffer-exit-hook)
  :config
  ;; Escape minibuffer with single escape
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

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
  :config
  (require 'saveplace)
  (setq-default save-place t))

(use-package smerge-mode
  :commands smerge-mode
  :custom
  (smerge-command-prefix (kbd "C-s"))
  :functions (ef-enable-smerge-maybe)
  :init
  (ef-add-hook (find-file-hook after-revert-hook) :fn ef-enable-smerge-maybe
    "Auto-enable `smerge-mode' when merge conflict is detected."
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil :noerror)
        (smerge-mode t)))))

(use-package whitespace
  :config
  (global-whitespace-mode 1)
  (setq-default show-trailing-whitespace nil)
  (setq whitespace-style (quote (face trailing))))

(provide 'core-pkg)
