;;; early-init.el -*- lexical-binding: t; -*-

;; Disable garbage collection completely during initialization. We will later
;; enable it again when we pull in `gcmh-mode'.
(setq gc-cons-threshold most-positive-fixnum)

(defconst ef-initial-load-prefer-newer load-prefer-newer
  "Initial value of `load-prefer-newer' at start-up time.")

(defconst ef-initial-file-name-handler-alist file-name-handler-alist
  "Initial value of `file-name-handler-alist' at start-up time.")

(defun +reset-load-prefer-newer-h ()
  "Resets `load-prefer-newer' to it's initial value."
  (setq-default load-prefer-newer ef-initial-load-prefer-newer))

(defun +reset-file-name-handler-alist-h ()
  "Resets `file-name-handler-alist' to it's initial value."
  (setq file-name-handler-alist
        ;; Merge instead of overwrite because there may have bene changes to
        ;; `file-name-handler-alist' since startup we want to preserve.
        (delete-dups (append file-name-handler-alist
                             ef-initial-file-name-handler-alist))))

(unless (or (daemonp) noninteractive init-file-debug)
  ;; Don't do mtime checks on `load' during init. We reset this later since
  ;; this may lead to unexpected surprises when doing devlopment work on elisp
  ;; files.
  (setq load-prefer-newer nil)

  ;; Avoid going through `file-name-handler-alist' every time we `require' or
  ;; `load' during start-up.
  (setq-default file-name-handler-alist nil)

  ;; Install hook to reset these again.
  (add-hook 'emacs-startup-hook #'+reset-file-name-handler-alist-h 101)
  (add-hook 'after-init-hook #'+reset-load-prefer-newer-h)

  ;; Disable GUI elements as early possible to avoid flickering during startup.
  ;;
  ;; We do this in early-init.el because the screen hasn't been drawn yet. An
  ;; alternative is setting `inhibit-redisplay', but then we'd just see a blank
  ;; screen when packages are being installed, and some sort of progress
  ;; information is nice to have.
  (when (and (fboundp 'scroll-bar-mode)) (scroll-bar-mode -1))
  (when (and (fboundp 'tool-bar-mode)) (tool-bar-mode -1))

  ;; Disable GTK tooltips if we're in X.
  (when (eq (window-system) 'x)
    (setq x-gtk-use-system-tooltips nil))

  ;; Keep menu-bar on macOS.
  (unless (memq (window-system) '(mac ns))
    (when (and (fboundp 'menu-bar-mode))
      (menu-bar-mode -1)))

  ;; We use straight.el to manage our packages.
  (setq package-enable-at-startup nil)

  ;; Don't load any system-wide site-lisp files.
  (setq-default site-run-file nil))

(provide 'early-init)
