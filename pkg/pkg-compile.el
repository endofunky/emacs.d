(use-package compile
  :defer t
  :config
  (setq compilation-always-kill t
        compilation-message-face 'default)

  (defun ef-compilation-exit-autoclose (status code msg)
    (when (and (eq status 'exit) (zerop code))
      (bury-buffer)
      (delete-window (get-buffer-window (get-buffer "*compilation*"))))
    (cons msg code))

  (setq compilation-exit-message-function 'ef-compilation-exit-autoclose))

(provide 'pkg-compile)
