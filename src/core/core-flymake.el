(require 'core-lib)
(require 'core-shackle)
(require 'core-evil)

(use-package flymake
  :straight nil
  :general
  (:keymap 'flymake-diagnostics-buffer-mode-map
   "M-e" 'quit-window)
  (:keymap 'flymake-mode-map
   "M-e" 'ef-flymake-toggle-errors)
  :custom
  (elisp-flymake-byte-compile-load-path (append load-path '("./")))
  (flymake-no-changes-timeout 0.2)
  :hook
  (prog-mode . flymake-mode)
  (evil-insert-state-exit . ef-flymake-check-buffer-maybe)
  (evil-replace-state-exit . ef-flymake-check-buffer-maybe)
  (ef-escape . ef-flymake-check-buffer-maybe)
  :config
  (ef-add-hook flymake-mode-hook
    (remove-hook 'flymake-diagnostic-functions 'elisp-flymake-checkdoc t))

  (ef-add-popup 'flymake-diagnostics-buffer-mode
                :size .2 :select nil :ephemeral t)

  (defun ef-flymake-follow-diagnostics-buffer (&rest args)
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
               #'ef-flymake-follow-diagnostics-buffer)

  (add-to-list 'window-selection-change-functions
               #'ef-flymake-follow-diagnostics-buffer)

  (defun ef-flymake-check-buffer-maybe ()
    (when (bound-and-true-p flymake-mode)
      (flymake-start)))

  (defun ef-flymake-toggle-errors ()
    (interactive)
    (if (eq major-mode 'flymake-diagnostics-buffer-mode)
        (quit-window)
      (if-let ((win (get-buffer-window (flymake--diagnostics-buffer-name))))
          (delete-window win)
        (flymake-show-buffer-diagnostics)))))

(provide 'core-flymake)
