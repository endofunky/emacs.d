(defconst ef-emacs-start-time (current-time))

(defun ef-init-debug-p ()
  "Returns true if the EMACS_INIT_DEBUG environment variable is set."
  (stringp (getenv "EMACS_INIT_DEBUG")))

;; Less warnings unless we're in debug mode.
(when (ef-init-debug-p)
  (setq-default warning-minimum-level :error))

(defconst ef-initial-gc-cons-threshold gc-cons-threshold
  "Initial value of `gc-cons-threshold' at start-up time.")

(defun ef-reset-gc-cons-threshold ()
  "Resets `gc-cons-threshold` to it's initial value"
  (setq-default gc-cons-threshold ef-initial-gc-cons-threshold))

(add-hook 'after-init-hook 'ef-reset-gc-cons-threshold)

(setq-default
;   Less GC breaks during start-up
 gc-cons-threshold most-positive-fixnum
;;  Increase *Messages* buffer size
 message-log-max 16384
 ;; Silence ad-handle-definition about advised functions getting redefined.
 ad-redefinition-action 'accept
 ;; Less byte-compiler noise
 byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local)
 byte-compile-verbose (ef-init-debug-p))

;; Remove menu items early to avoid flickering.
(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(menu-bar-mode -1)

(let ((default-directory (expand-file-name "elpa" user-emacs-directory)))
  (unless (file-exists-p default-directory)
    (make-directory default-directory))
  (normal-top-level-add-subdirs-to-load-path))

(unless (fboundp 'package-installed-p)
  (package-initialize t))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(customize-set-variable 'use-package-enable-imenu-support t)
(require 'use-package)

;; Ensure the :diminish keyword passed to use-package gets respected
(use-package diminish :ensure t)

(if (ef-init-debug-p)
    (setq-default use-package-verbose t))

(add-to-list 'load-path (expand-file-name "vendor" user-emacs-directory))

(defun ef-require-directory-files (dir)
  (mapc (lambda (name)
          (require (intern (file-name-sans-extension name))))
        (directory-files dir nil "\\.el$")))

(dolist (path '("core" "pkg" "lang" "private"))
  (let ((absolute-path (expand-file-name path user-emacs-directory)))
    (add-to-list 'load-path absolute-path)
    (ef-require-directory-files absolute-path)))

(unless noninteractive
  (let ((elapsed (float-time (time-subtract (current-time)
                                            ef-emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed)))

(provide 'init)
