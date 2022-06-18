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

  ;; Hide commands in M-x which do not work in the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)

  ;; Keep auto-save files separately
  (auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

  ;; Don't show GUI dialog boxes
  (use-dialog-box nil))

;; Enable y/n answers
(fset 'yes-or-no-p #'y-or-n-p)

;; No GNU ads in minibuffer
(fset #'display-startup-echo-area-message #'ignore)

;; Show columns in mode-line
(column-number-mode t)

;; Never use TUI pagers in sub-processes
(setenv "PAGER" (executable-find "cat"))

(use-package ansi-color
  :after comint
  :custom
  (ansi-color-for-comint-mode t)
  :defer t)

(use-package autorevert
  :custom
  (auto-revert-interval 1)
  (auto-revert-verbose nil)
  (auto-revert-check-vc-info t)
  (global-auto-revert-non-file-buffers t)
  :config
  (global-auto-revert-mode 1))

(use-package calc
  :defer t
  :custom
  (math-additional-units
   '((GiB "1024 * MiB" "Giga Byte")
     (MiB "1024 * KiB" "Mega Byte")
     (KiB "1024 * B" "Kilo Byte")
     (B nil "Byte")
     (Gib "1024 * Mib" "Giga Bit")
     (Mib "1024 * Kib" "Mega Bit")
     (Kib "1024 * b" "Kilo Bit")
     (b "B / 8" "Bit"))))

(use-package cus-edit
  :straight nil
  :defer t
  :after no-littering
  :custom
  (custom-file (no-littering-expand-etc-file-name "custom.el")))

(use-package comint
  :straight nil
  :defer t
  :custom
  (comint-buffer-maximum-size 2048)
  (comint-move-point-for-output t)
  (comint-prompt-read-only t)
  :commands (comint-truncate-buffer)
  :hook (compilation-filter . comint-truncate-buffer)
  :general
  (:states 'insert :keymaps 'comint-mode-map
   "<up>" 'comint-previous-input
   "<down>" 'comint-next-input)
  :config
  (evil-set-initial-state 'comint-mode 'normal)
  (ef-add-hook comint-mode-hook
    "Do not use `truncate-lines' in comint buffers.."
    (setq-local truncate-lines nil)
    (set (make-local-variable 'truncate-partial-width-windows) nil)))

(use-package comp
  :straight nil
  :when (fboundp 'native-comp-available-p)
  :custom
  (native-comp-async-query-on-exit t)
  (native-comp-async-report-warnings-errors
   (stringp (getenv "EMACS_INIT_DEBUG"))))

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
  (compilation-filter . ef-colorize-compilation-buffer)
  :config
  (require 'ansi-color)
  (defun ef-colorize-compilation-buffer ()
    (read-only-mode nil)
    (ansi-color-apply-on-region compilation-filter-start (point))
    (read-only-mode t))

  (ef-add-hook compilation-mode-hook
    "Enable `visual-line-mode' for compilation buffers."
    (setq-local bidi-display-reordering nil)
    (visual-line-mode t)))

;; Do not allow the cursor in the minibuffer prompt
(use-package cursor-sensor
  :custom
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  :hook
  (minibuffer-setup . cursor-intangible-mode))

(use-package delsel
  :config
  ;; Type over region: Treat an Emacs region much like a typical selection
  ;; outside of Emacs
  (delete-selection-mode t))

(use-package display-line-numbers
  :commands (display-line-numbers-mode)
  :custom
  (display-line-numbers-width-start t))

(use-package ibuffer
  :defer t
  :general
  (:states 'normal :keymaps 'ibuffer-mode-map
   "q" 'kill-this-buffer))

(use-package files
  :straight nil
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
  :custom
  (hl-line-sticky-flag nil)
  :hook
  (prog-mode . hl-line-mode)
  (text-mode . hl-line-mode))

(use-package hideshow
  :commands hs-minor-mode
  :hook (prog-mode . hs-minor-mode))

(use-package minibuffer
  :straight nil
  :custom
  (enable-recursive-minibuffers t)
  :config
  (ef-add-hook minibuffer-setup-hook
    (set-window-fringes (get-buffer-window (current-buffer)) 0 0))

  ;; Escape minibuffer with single escape
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit))

(use-package prog-mode
  :straight nil
  :general
  (:states 'normal :keymaps 'prog-mode-map
   "<tab>" 'indent-for-tab-command)
  (:states 'normal :prefix ef-prefix :keymaps 'prog-mode-map
   "<tab>" '(ef-indent-buffer :wk "Indent Buffer")
   "." '(pop-tag-mark :wk "Pop Tag Mark"))
  (:states 'visual :keymaps 'prog-mode-map
   "<tab>" 'indent-region)
  :config
  (ef-add-hook prog-mode-hook
    (setq-local show-trailing-whitespace t))

  (global-prettify-symbols-mode -1))

(use-package recentf
  :defer t
  :after no-littering
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package savehist
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
  :straight nil
  :defer t
  :config
  (setq tabify-regexp "^\t* [ \t]+"))

(use-package text-mode
  :straight nil
  :defer t
  :mode (("/LICENSE\\'" . text-mode)
         ("\\.log\\'" . text-mode))
  :config
  (ef-add-hook text-mode-hook
    (setq-local show-trailing-whitespace t)))

(use-package transient
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
  (transient-show-popup t)
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
  :straight nil
  :custom
  (uniquify-after-kill-buffer-p t)
  (uniquify-buffer-name-style 'post-forward-angle-brackets)
  (uniquify-ignore-buffers-re "^\\*")
  (uniquify-separator ":"))

(use-package xref
  :straight nil
  :defer t
  :custom
  (xref-marker-ring-length 1024)
  :config
  (ef-add-popup "*xref*")

  (defadvice xref-goto-xref (after my activate)
    (delete-window (get-buffer-window (get-buffer "*xref*")))))

(provide 'core-emacs)
