;;; early-init.el -*- lexical-binding: t; -*-

;; Disable garbage collection completely during initialization. We will later
;; enable it again when we pull in `gcmh-mode'.
(setq gc-cons-threshold most-positive-fixnum)

(defconst ef-initial-load-prefer-newer load-prefer-newer
  "Initial value of `load-prefer-newer' at start-up time.")

(defconst ef-initial-file-name-handler-alist file-name-handler-alist
  "Initial value of `file-name-handler-alist' at start-up time.")

(defun +reset-startup-values-h ()
  "Resets early-init.el performance overrides to their initial values."
  (setq-default file-name-handler-alist ef-initial-file-name-handler-alist
                load-prefer-newer ef-initial-load-prefer-newer))

(unless (or (daemonp) noninteractive)
  (unless init-file-debug
    ;; Don't do mtime checks on `load' during init. We reset this later since
    ;; this may lead to unexpected surprises when doing devlopment work on elisp
    ;; files.
    (customize-set-variable 'load-prefer-newer nil)

    ;; Avoid going through `file-name-handler-alist' every time we `require' or
    ;; `load' during start-up.
    (setq-default file-name-handler-alist nil)

    ;; Install hook to reset these again.
    (add-hook 'after-init-hook #'+reset-startup-values-h))

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
    (customize-set-variable 'x-gtk-use-system-tooltips nil))

  ;; Keep menu-bar on macOS.
  (unless (memq (window-system) '(mac ns))
    (when (and (fboundp 'menu-bar-mode))
      (menu-bar-mode -1)))

  ;; We use straight.el to manage our packages.
  (customize-set-variable 'package-enable-at-startup nil)

  ;; Don't load any system-wide site-lisp files.
  ;;
  ;; We have to use setq here since the customize setter will prevent us from
  ;; modifying this.
  (setq-default site-run-file nil))

(provide 'early-init)
