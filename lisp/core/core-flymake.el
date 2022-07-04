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

  (poe-popup 'flymake-diagnostics-buffer-mode
                :size .2 :select nil :ephemeral t)

  (defun +flymake-follow-diagnostics-buffer (&rest args)
    "Window hook function that checks if the current buffer has `flymake-mode'
enabled, the flymake diagnostics buffer is visible, and if so will follow the
diagnostics to the buffer being switched to."
    (when (and (not (eq major-mode 'flymake-diagnostics-buffer-mode))
               flymake-mode
               (cl-some #'(lambda (win)
                            (with-current-buffer (window-buffer win)
                              (eq major-mode 'flymake-diagnostics-buffer-mode)))
                        (window-list)))
      (flymake-show-buffer-diagnostics)))

  (add-to-list 'window-buffer-change-functions
               #'+flymake-follow-diagnostics-buffer)

  (add-to-list 'window-selection-change-functions
               #'+flymake-follow-diagnostics-buffer)

  (defun +flymake-check-buffer-maybe ()
    (when (bound-and-true-p flymake-mode)
      (flymake-start)))

  (defun +flymake-toggle-errors ()
    (interactive)
    (if (eq major-mode 'flymake-diagnostics-buffer-mode)
        (quit-window)
      (if-let ((win (get-buffer-window (flymake--diagnostics-buffer-name))))
          (delete-window win)
        (flymake-show-buffer-diagnostics))))

  ;; Disable flymake while in insert or replace state
  (defvar ef--flymake-delay nil)

  (defun +flymake-disable ()
    (setq ef--flymake-delay flymake-no-changes-timeout)
    (setq flymake-no-changes-timeout nil))

  (defun +flymake-enable ()
    (setq flymake-no-changes-timeout ef--flymake-delay)
    (when (bound-and-true-p flymake-mode)
      (flymake-start))))

(provide 'core-flymake)
