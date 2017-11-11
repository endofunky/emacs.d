;; Tabs are just awful
(setq-default indent-tabs-mode nil)

;; Disable bell completely
(setq ring-bell-function 'ignore)

;; Keep the filesystem clean
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Treat an Emacs region much like a typical selection outside of Emacs
(delete-selection-mode t)

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

;; Don't mess with the init.el
(setq custom-file "~/.emacs.d/custom.el")

;; UTF-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(setq buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; Show columns in mode-line
(column-number-mode t)

;; Escape minibuffer with single escape
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; Never delete the scratch buffer
(defun ef-get-scratch-buffer-create ()
  "Get *scratch* buffer or create it."
  (unless (get-buffer "*scratch*")
    (with-current-buffer (generate-new-buffer "*scratch*")
      (insert initial-scratch-message)
      (set-buffer-modified-p nil)
      (funcall initial-major-mode))))

(run-with-idle-timer 1 t 'ef-get-scratch-buffer-create)

;; Empty scratch buffer by default
(setq initial-scratch-message "")

;;; Enable disabled commands

(setq disabled-command-function nil)
(put 'LaTeX-narrow-to-environment 'disabled nil)
(put 'TeX-narrow-to-group         'disabled nil)
(put 'company-coq-fold            'disabled nil)
(put 'downcase-region             'disabled nil)   ; Let downcasing work
(put 'erase-buffer                'disabled nil)
(put 'eval-expression             'disabled nil)   ; Let ESC-ESC work
(put 'narrow-to-page              'disabled nil)   ; Let narrowing work
(put 'narrow-to-region            'disabled nil)   ; Let narrowing work
(put 'scroll-left                 'disabled nil)
(put 'set-goal-column             'disabled nil)
(put 'upcase-region               'disabled nil)   ; Let upcasing work

;; Never use TUI pagers in sub-processes
(setenv "PAGER" (executable-find "cat"))

;; Ignore unsafe local variables
(setq enable-local-variables :safe)
(provide 'core-emacs)
