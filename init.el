(defun ef-init-debug-p ()
  "Returns true if the EMACS_INIT_DEBUG environment variable is set."
  (stringp (getenv "EMACS_INIT_DEBUG")))

;; Less warnings unless we're in debug mode.
(when (ef-init-debug-p)
  (setq-default warning-minimum-level :error))

(setq-default
 ;; Increase *Messages* buffer size
 message-log-max 16384
 ;; Silence ad-handle-definition about advised functions getting redefined.
 ad-redefinition-action 'accept
 ;; Less byte-compiler noise
 byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local)
 byte-compile-verbose (ef-init-debug-p))

;; Initialize elpa load path
(let ((default-directory (expand-file-name "elpa" user-emacs-directory)))
  (unless (file-exists-p default-directory)
    (make-directory default-directory))
  (normal-top-level-add-subdirs-to-load-path))

(custom-set-variables '(straight-check-for-modifications
                        '(check-on-save find-when-checking)))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'straight)
(custom-set-variables '(use-package-enable-imenu-support t))
(custom-set-variables '(straight-use-package-by-default t))

(straight-use-package 'use-package)

(require 'use-package)

(when (ef-init-debug-p)
  (setq-default use-package-verbose t))

;; Require no-littering as early as possible so we don't end up storing files
;; before our directory structure has been set up.
(use-package no-littering
  :config
  (setq no-littering-etc-directory
        (expand-file-name "etc/" user-emacs-directory))
  (setq no-littering-var-directory
        (expand-file-name "var/" user-emacs-directory)))

(let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
  (normal-top-level-add-subdirs-to-load-path))

(let ((default-directory (expand-file-name "lisp" user-emacs-directory)))
  (normal-top-level-add-subdirs-to-load-path)
  (dolist (file (directory-files-recursively default-directory "\\.el$"))
    (with-demoted-errors "Require failed: %S"
      (require (intern (file-name-sans-extension (file-name-base file))) nil t))))

(provide 'init)
