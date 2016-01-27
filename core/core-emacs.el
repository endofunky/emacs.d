;; Tabs are just awful
(setq-default indent-tabs-mode nil)

;; Disable the fringe
(fringe-mode 0)
(set-fringe-style -1)

;; Disable bell completely
(setq ring-bell-function 'ignore)

;; Keep the filesystem clean
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Treat an Emacs region much like a typical selection outside of Emacs
(delete-selection-mode t)

;; Don't have a blinking cursor
(blink-cursor-mode -1)

;; Paren highlighting
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Scroll one line at a time
(setq scroll-margin 0)
(setq scroll-conservatively 100000)
(setq scroll-preserve-screen-position 1)

;; No splash screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; No GNU ads in minibuffer
(fset #'display-startup-echo-area-message #'ignore)

;; Don't wrap long lines
(setq-default truncate-lines -1)

;; UTF-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(setq default-buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; Pretify symbols
(global-prettify-symbols-mode 1)

;; Don't auto-split windows
(setq split-window-preferred-function 'ignore)

;; Highlight current line
(global-hl-line-mode 1)

;; Show columns in mode-line
(column-number-mode t)

;; Keep buffers in sync
(global-auto-revert-mode 1)

;; Don't defer screen updates when performing operations
(setq redisplay-dont-pause t)

;; Escape minibuffer with single escape
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(use-package savehist
  :defer 1
  :config
  (savehist-mode t)
  (setq savehist-file (expand-file-name "savehist" user-emacs-directory)
        savehist-additional-variables '(search ring regexp-search-ring)
        savehist-autosave-interval 60
        history-length 1000))

(use-package saveplace
  :defer 1
  :config
  (require 'saveplace)
  (setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
  (setq-default save-place t))

(use-package whitespace
  :config
  (global-whitespace-mode 1)
  (setq-default show-trailing-whitespace nil)
  (setq whitespace-style (quote (face trailing)))
  :diminish (whitespace-mode global-whitespace-mode))

;; Never delete the scratch buffer
(defun ts/get-scratch-buffer-create ()
  "Get *scratch* buffer or create it."
  (get-buffer-create "*scratch*"))

(run-with-idle-timer 1 t 'ts/get-scratch-buffer-create)

;; Empty scratch buffer by default
(setq initial-scratch-message "")

(provide 'core-emacs)
