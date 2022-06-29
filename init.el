;;; init.el -*- lexical-binding: t; -*-

;;
;; Display initialization time statistics in echo area
;;

(defconst ef-emacs-start-time (current-time)
  "Emacs startup time before loading any configuration.")

(defun +post-init-stats-message-h ()
  "Prints initialization statistics to the echo area.

Called from `after-init-hook'."
  (let ((elapsed (float-time (time-subtract (current-time)
                                            ef-emacs-start-time))))
    (message "Loading emacs done in %.3fs (%d garbage collection(s))"
             elapsed gcs-done)))

(unless (or (daemonp) noninteractive)
  (add-hook 'after-init-hook #'+post-init-stats-message-h))

;;
;; Bootstrap package and configuration management libraries.
;;
;; To download packages we use:
;;   https://github.com/radian-software/straight.el/
;;
;; To require and configure them:
;; https://github.com/jwiegley/use-package
;;

;; Override when straight.el checks if package files have been modified. By
;; default this includes find-at-startup which massively slows down emacs'
;; start-up.
(custom-set-variables '(straight-check-for-modifications
                        '(check-on-save find-when-checking)))

;; Native-compilation configuration. We have to do this EARLY, as in, before
;; straight.el and use-package get bootstrapped because otherwise we end up
;; recompiling them every time we start emacs.
(let ((eln-cache (expand-file-name "var/eln-cache/" user-emacs-directory)))
  (unless (file-exists-p eln-cache)
    (make-directory eln-cache))

  ;; Move native-compilation cache files to var directory.
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (convert-standard-filename eln-cache))))

;; Don't display native-compilation warnings unless in debug-mode.
(setq-default native-comp-async-report-warnings-errors init-file-debug)

;; Warn users on exit when native-compilation is running.
(setq-default native-comp-async-query-on-exit t)

;; We use no-littering below to set up our directory structure for package-
;; related files, so stick with that.
;;
;; Have to explicitly use defvar here so the bootstrap process picks this up.
(defvar straight-base-dir (expand-file-name "var" user-emacs-directory))

(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "var/straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'straight)

;; Keep straight.el's version lock file outside of the var directory so we can
;; easrily track it in git.
(let ((version-file (expand-file-name "versions.el" user-emacs-directory)))
  (custom-set-variables `(straight-profiles '((nil . ,version-file)))))

;; Show use-package statements in `imenu'.
(custom-set-variables '(use-package-enable-imenu-support t))

;; We don't want to have to write :straight t for every package, so make it
;; the default.
(custom-set-variables '(straight-use-package-by-default t))

;; Install use-package.
(straight-use-package 'use-package)

(require 'use-package)

;; Display package load messages if we start in debug mode.
(setq-default use-package-verbose init-file-debug)

;; A lot of packages create files in random places in ~/.emacs.d. We want to
;; avoid that and store them neatly in ~/.emacs.d/etc and ~/.emacs.d/var. We
;; have to do that before any of the offending packages are required, so we
;; might a well do that here.
(let ((etc (expand-file-name "etc" user-emacs-directory))
      (var (expand-file-name "var" user-emacs-directory)))
  ;; Ensure the etc directory exist. The var directory will already have been
  ;; created when configuring native-compilation, so let's not waste time
  ;; checking for that one again.
  (unless (file-exists-p etc)
    (make-directory etc))

  (use-package no-littering
    :demand t
    :commands (no-littering-expand-var-file-name
               no-littering-expand-etc-file-name)
    :config
    ;; Keep auto-save files separately in our var directory.
    (setq auto-save-file-name-transforms
	  `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

    ;; And do the same for backup files ...
    (setq backup-directory-alist
          `(("." . ,(no-littering-expand-var-file-name "backup/"))))

    ;; Keep customize variables in the etc/ directory instead of .emacs.d.
    (setq custom-file (no-littering-expand-etc-file-name "custom.el"))

    ;; Both of these are declared using `defvar'.
    (setq no-littering-etc-directory etc)
    (setq no-littering-var-directory var)

    ;; If recentf gets loaded sometime down the line, exclude the etc and var
    ;; directories from it's history.
    (defvar recentf-exclude)
    (with-eval-after-load 'recentf
      (add-to-list 'recentf-exclude var)
      (add-to-list 'recentf-exclude etc))))

;; We disabled garbage collection in early-init.el. We don't re-enable it
;; manually, but here we use the gcmh package to manage garbage collection
;; thresholds for us instead.
(use-package gcmh
  :demand t
  :commands (gcmh-mode)
  :hook
  (after-init . gcmh-mode))

;; Load all modules from lisp/.
;;
;; We intentionally don't place any attention to the exact order to requiring
;; the files here since they should explicitly require their dependencies
;; anyway.
(let ((default-directory (expand-file-name "lisp" user-emacs-directory)))
  (normal-top-level-add-subdirs-to-load-path)
  (dolist (file (directory-files-recursively default-directory "\\.el$"))
    (with-demoted-errors "Require failed: %S"
      (require (intern (file-name-sans-extension (file-name-base file)))
               nil t))))
