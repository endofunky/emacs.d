;;; core-flymake.el --- Flymake syntax checker -*- lexical-binding: t; -*-
(require 'core-lib)
(require 'core-popup)
(require 'core-evil)

(use-package flymake
  :straight nil
  :general
  (:keymap 'flymake-diagnostics-buffer-mode-map
   "M-e" 'quit-window)
  (:keymap 'flymake-mode-map
   "M-e" '+flymake-toggle-errors)
  :custom
  (elisp-flymake-byte-compile-load-path (append load-path '("./")))
  (flymake-no-changes-timeout 0.1)
  ;; Don't syntax check when compilation is running.
  (flymake-proc-compilation-prevents-syntax-check t)
  ;; Don't wrap around buffer boundaries
  (flymake-wrap-around nil)
  :hook
  (prog-mode . flymake-mode)
  (evil-insert-state-entry . +flymake-disable)
  (evil-replace-state-entry . +flymake-disable)
  (evil-insert-state-exit . +flymake-enable)
  (evil-replace-state-exit . +flymake-enable)
  (ef-escape . +flymake-check-buffer-maybe)
  :config
  (+add-hook flymake-mode-hook
    (remove-hook 'flymake-diagnostic-functions 'elisp-flymake-checkdoc t))

  (poe-popup 'flymake-diagnostics-buffer-mode :size .2 :select nil)

  (defun +is-flymake-disagnostics-buffer-window-p (window)
    "Predicate function that checks whether WINDOW contains a
`flymake-diagnostics-buffer-mode' buffer.

Returns the WINDOW or nil."
    (when (eq (buffer-local-value 'major-mode (window-buffer window))
              'flymake-diagnostics-buffer-mode)
      window))

  (defun +find-flymake-diagnostics-buffer-window ()
    "Find a FlyMake diagnostics buffer window in `window-list'.

Returns the window if found or nil otherwise."
    (cl-some #'+is-flymake-disagnostics-buffer-window-p (window-list)))

  (defun +flymake-follow-diagnostics-buffer (&rest args)
    "Window hook function that checks if the current buffer has
`flymake-mode'enabled, the flymake diagnostics buffer is visible,
and if so will follow the diagnostics to the buffer being switched
to."
    (when (and (not (eq major-mode 'flymake-diagnostics-buffer-mode))
               flymake-mode)
      (when-let ((existing-window (+find-flymake-diagnostics-buffer-window)))
        (kill-buffer (window-buffer existing-window))
        (flymake-show-buffer-diagnostics))))

  (add-to-list 'window-buffer-change-functions
               #'+flymake-follow-diagnostics-buffer)

  (add-to-list 'window-selection-change-functions
               #'+flymake-follow-diagnostics-buffer)

  (defun +flymake-check-buffer-maybe ()
    "Run `flymake-start' if `flymake-mode' is enabled."
    (when (bound-and-true-p flymake-mode)
      (flymake-start)))

  (defun +flymake-toggle-errors ()
    "Toggle the FlyMake diagnostics window."
    (interactive)
    (if (eq major-mode 'flymake-diagnostics-buffer-mode)
        (quit-window)
      (if-let ((win (+find-flymake-diagnostics-buffer-window)))
          (delete-window win)
        (flymake-show-buffer-diagnostics))))

  ;; Disable flymake while in insert or replace state
  (defvar ef--flymake-delay nil
    "Temporary variable to store `flymake-no-changes-timeout' when
disabling automatic checks with `+flymake-disable'.")

  (defun +flymake-disable ()
    "Disable automatic checks by FlyMake.

Works by setting `flymake-no-changes-timeout' to nil. Stores the
current value in `ef--flymake-delay'."
    (setq ef--flymake-delay flymake-no-changes-timeout)
    (setq flymake-no-changes-timeout nil))

  (defun +flymake-enable ()
    "Re-eable automatic checks by FlyMake previously disabled by
`+flymake-disable'.

Restores `flymake-no-changes-timeout' from `ef--flymake-delay'."
    (setq flymake-no-changes-timeout ef--flymake-delay)
    (when (bound-and-true-p flymake-mode)
      (flymake-start))))

(provide 'core-flymake)
