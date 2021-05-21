(require 'core-evil)
(require 'core-lib)
(require 'core-shackle)

(use-package no-littering :defer t)

(ef-customize
  (fill-column 80)

  ;; Word wrap at spaces
  (word-wrap t)

  ;; Display keystrokes in echo area as soon as possible.
  ;; Note: Setting this to a value < 0 has the same effect as setting it to
  ;; 0 since (at least) Emacs 25, disabling to completely.
  (echo-keystrokes 0.0001)

  ;; `whitespace-mode' doesn't play nice with `company-mode' overlays, also
  ;; `show-trailing-whitespace' is implemented in C, so should be more
  ;; performant.
  (show-trailing-whitespace t)

  ;; Tabs are just awful
  (indent-tabs-mode nil)

  ;; Disable bell completely
  (ring-bell-function 'ignore)

  ;; Keep the filesystem clean
  (make-backup-files nil)
  (auto-save-default nil)

  ;; Scroll one line at a time
  (scroll-margin 0)
  (scroll-conservatively 100000)
  (scroll-preserve-screen-position 1)

  ;; Don't display splash screen after start up
  (inhibit-splash-screen t)
  (inhibit-startup-message t)

  ;; Don't wrap long lines
  (truncate-lines -1)

  ;; Empty scratch buffer by default
  (initial-scratch-message "")

  ;; Ignore unsafe local variables
  (enable-local-variables :safe)

  ;; Keep auto-save files separately
  (auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; Enable y/n answers
(fset 'yes-or-no-p #'y-or-n-p)

;; No GNU ads in minibuffer
(fset #'display-startup-echo-area-message #'ignore)

;; Show columns in mode-line
(column-number-mode t)

;; Never use TUI pagers in sub-processes
(setenv "PAGER" (executable-find "cat"))

(use-package autorevert
  :custom
  (auto-revert-interval 1)
  (auto-revert-verbose nil)
  (global-auto-revert-non-file-buffers t)
  :config
  (global-auto-revert-mode 1))

(use-package cus-edit
  :defer t
  :after no-littering
  :custom
  (custom-file (no-littering-expand-etc-file-name "custom.el")))

(use-package comint
  :defer t
  :custom
  (comint-buffer-maximum-size 2048)
  (comint-move-point-for-output 'others)
  (comint-prompt-read-only t)
  :commands (comint-truncate-buffer)
  :general
  (:states 'insert :keymaps 'comint-mode-map
   "<up>" 'comint-previous-input
   "<down>" 'comint-next-input)
  :config
  (ef-add-hook comint-mode-hook
    "Do not use `truncate-lines' in comint buffers.."
    (setq-local truncate-lines nil)
    (set (make-local-variable 'truncate-partial-width-windows) nil)))

(use-package compile
  :defer t
  :custom
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  (compilation-message-face 'default)
  (compilation-scroll-output 'first-error)
  :functions (ef-compilation-exit-autoclose)
  :hook
  ;; Avoid dropping into insert mode in compilation windows
  (compilation-start . evil-normal-state)
  (compilation-filter . comint-truncate-buffer)
  :config
  (ef-add-hook compilation-mode-hook
    "Enable `visual-line-mode' for compilation buffers."
    (setq-local bidi-display-reordering nil)
    (visual-line-mode t)))

(use-package delsel
  :config
  ;; Type over region: Treat an Emacs region much like a typical selection
  ;; outside of Emacs
  (delete-selection-mode t))

(use-package display-line-numbers
  :commands (display-line-numbers-mode)
  :custom
  (display-line-numbers-width-start t))

(use-package files
  :unless noninteractive
  :config
  (setq save-silently t)
  (ef-add-hook after-save-hook :fn ef-after-save-message-hook
    (message "\"%s\" %dL, %dC written"
	     (buffer-name)
	     (count-lines (point-min) (point-max))
	     (buffer-size))))

(use-package hl-line
  :defer t
  :hook
  (prog-mode . hl-line-mode)
  (text-mode . hl-line-mode))

(use-package hideshow
  :commands hs-minor-mode
  :hook (prog-mode . hs-minor-mode))

(use-package minibuffer
  :custom
  (enable-recursive-minibuffers t)
  :config
  ;; Escape minibuffer with single escape
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit))

(use-package prog-mode
  :general
  (:states 'normal :keymaps 'prog-mode-map
   "<tab>" 'indent-for-tab-command)
  (:states 'normal :prefix ef-prefix :keymaps 'prog-mode-map
   "<tab>" '(ef-indent-buffer :wk "Indent Buffer")
   "." '(pop-tag-mark :wk "Pop Tag Mark"))
  (:states 'visual :keymaps 'prog-mode-map
   "<tab>" 'indent-region)
  :config
  (global-prettify-symbols-mode -1))

(use-package recentf
  :defer t
  :after no-littering
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package savehist
  :defer 1
  :custom
  (history-length 1000)
  (savehist-additional-variables '(search ring regexp-search-ring))
  (savehist-autosave-interval 60)
  (savehist-save-minibuffer-history t)
  (savehist-additional-variables '(kill-ring
                                   register-alist
                                   mark-ring global-mark-ring
                                   search-ring
                                   regexp-search-ring))
  :config
  (savehist-mode t))

(use-package saveplace
  :custom
  (save-place-limit nil)
  :init
  (save-place-mode t)
  :config
  (setq-default save-place t))

(use-package smerge-mode
  :commands smerge-mode
  :custom
  (smerge-command-prefix (kbd "C-s"))
  :init
  (ef-add-hook (find-file-hook after-revert-hook) :fn ef-enable-smerge-maybe
    "Auto-enable `smerge-mode' when merge conflict is detected."
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil :noerror)
        (smerge-mode t)))))

(use-package so-long
  :config
  (global-so-long-mode 1))

(use-package tabify
  :defer t
  :config
  (setq tabify-regexp "^\t* [ \t]+"))

(use-package text-mode
  :defer t
  :mode (("/LICENSE\\'" . text-mode)
         ("\\.log\\'" . text-mode)))

(use-package transient
  :ensure t                             ; emacs < 28
  :general
  (:keymaps '(transient-map transient-edit-map)
   "<escape>" 'transient-quit-all
   "?" 'transient-show
   "C-h" 'transient-show
   "C-t" 'transient-help)
  :commands (transient-define-prefix
              transient-bind-q-to-quit)
  :custom
  (transient-enable-popup-navigation t)
  (transient-show-popup 1)
  :config
  (transient-bind-q-to-quit)

  (defadvice transient-setup (before transient-setup activate)
    (ef-transient-suspend-shackle-mode))

  (defun ef-transient-suspend-shackle-mode ()
    (when (bound-and-true-p shackle-mode)
      (shackle-mode -1)
      (add-hook 'transient-exit-hook 'ef-transient-resume-shackle-mode)))

  (defun ef-transient-resume-shackle-mode ()
    (unless transient--prefix
      (shackle-mode t)
      (remove-hook 'transient-exit-hook 'ef-transient-resume-shackle-mode))))

(use-package uniquify
  :custom
  (uniquify-after-kill-buffer-p t)
  (uniquify-buffer-name-style 'post-forward-angle-brackets)
  (uniquify-ignore-buffers-re "^\\*")
  (uniquify-separator ":"))

(use-package vc-mode
  :defer t
  :init
  ;; Most repositories are using git nowadays, so remove the 'Git-' prefix from
  ;; mode-line-format. If we are in a non-git repository, this will still be
  ;; indicated.
  (setcdr (assq 'vc-mode mode-line-format)
          '((:eval (replace-regexp-in-string "^ Git[-:]" " " vc-mode)))))

(use-package xref
  :defer t
  :custom
  (xref-marker-ring-length 1024)
  :config
  (ef-add-popup "*xref*")

  (defadvice xref-goto-xref (after my activate)
    (delete-window (get-buffer-window (get-buffer "*xref*")))))

(provide 'core-emacs)
