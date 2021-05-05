(require 'core-lib)

(ef-customize
  ;; Tabs are just awful
  (indent-tabe-mode nil)

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

  ;; Don't store customizations in init.el
  (custom-file (expand-file-name "custom.el" user-emacs-directory))

  ;; Empty scratch buffer by default
  (initial-scratch-message "")

  ;; Ignore unsafe local variables
  (enable-local-variables :safe))


(use-package delsel
  :config
  ;; Treat an Emacs region much like a typical selection outside of Emacs
  (delete-selection-mode t))

;; Enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; No GNU ads in minibuffer
(fset #'display-startup-echo-area-message #'ignore)

;; Show columns in mode-line
(column-number-mode t)

;; Never use TUI pagers in sub-processes
(setenv "PAGER" (executable-find "cat"))

(provide 'core-emacs)
