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

(dolist (path '("core" "lang" "pkg" "vendor"))
  (add-to-list 'load-path (expand-file-name path user-emacs-directory)))

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

;; Load evil-mode early in case something goes wrong and so package
;; configurations can assume evil functions to be available.
(require 'pkg-evil)

(require 'core-emacs)
(require 'core-gui)
(require 'core-mac)

(require 'pkg-ag)
(require 'pkg-auto-complete)
(require 'pkg-eldoc)
(require 'pkg-ido)
(require 'pkg-magit)
(require 'pkg-popwin)
(require 'pkg-projectile)
(require 'pkg-rainbow-delimiters)
(require 'pkg-restclient)
(require 'pkg-smartparens)
(require 'pkg-spaceline)
(require 'pkg-theme)

(require 'lang-c)
(require 'lang-coffee)
(require 'lang-css)
(require 'lang-dockerfile)
(require 'lang-dot)
(require 'lang-js)
(require 'lang-json)
(require 'lang-lisp)
(require 'lang-llvm)
(require 'lang-markdown)
(require 'lang-ruby)
(require 'lang-shell)
(require 'lang-slim)
(require 'lang-thrift)
(require 'lang-web)
(require 'lang-yaml)

(let ((elapsed (float-time (time-subtract (current-time)
                                            ts/emacs-start-time))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))

(provide 'init)
