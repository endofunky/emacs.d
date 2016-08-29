(defconst ts/emacs-start-time (current-time))

;; Less warnings unless we're in debug mode.
(unless (getenv "EMACS_INIT_DEBUG")
  (setq warning-minimum-level :error))

(defconst ts/initial-gc-cons-threshold gc-cons-threshold
  "Initial value of `gc-cons-threshold' at start-up time.")

(defun ts/reset-gc-cons-threshold ()
  "Resets `gc-cons-threshold` to it's initial value"
  (setq gc-cons-threshold ts/initial-gc-cons-threshold))

(add-hook 'after-init-hook 'ts/reset-gc-cons-threshold)

(setq gc-cons-threshold (* 128 1024 1024))

;; Increate *Messages* buffer size
(setq message-log-max 16384)

;; Silence ad-handle-definition about advised functions getting redefined.
(setq ad-redefinition-action 'accept)

;; Remove menu items early to avoid flickering.
(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(menu-bar-mode -1)

(let ((default-directory (expand-file-name "elpa" user-emacs-directory)))
  (unless (file-exists-p default-directory)
        (make-directory default-directory))
  (normal-top-level-add-subdirs-to-load-path))

(setq package-enable-at-startup nil
      package-archives
      '(("melpa-stable" . "http://stable.melpa.org/packages/")
        ("melpa"        . "http://melpa.org/packages/")
        ("marmalade"    . "http://marmalade-repo.org/packages/")
        ("org"          . "http://orgmode.org/elpa/")
        ("gnu"          . "http://elpa.gnu.org/packages/")))

(package-initialize t)

(eval-when-compile
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package))

(if (getenv "EMACS_INIT_DEBUG")
    (setq use-package-verbose t))

(add-to-list 'load-path (expand-file-name "vendor" user-emacs-directory))

(defun ts/require-directory-files (dir)
  (mapc (lambda (name)
          (require (intern (file-name-sans-extension name))))
        (directory-files dir nil "\\.el$")))

(dolist (path '("core" "pkg" "lang"))
  (let ((absolute-path (expand-file-name path user-emacs-directory)))
    (add-to-list 'load-path absolute-path)
    (ts/require-directory-files absolute-path)))

(let ((elapsed (float-time (time-subtract (current-time)
                                            ts/emacs-start-time))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))

(provide 'init)
