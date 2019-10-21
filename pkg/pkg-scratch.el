(use-package persistent-scratch
  :ensure t
  :defer 1
  :config
  (persistent-scratch-setup-default))

(use-package evil
  :defer t
  :config
  (defvar ef-toggle-scratch--prev-buffer nil)

  (defun ef-toggle-scratch--goto-scratch ()
    (if-let* ((scratch-buffer (get-buffer "*scratch*")))
        (progn
          (setq ef-toggle-scratch--prev-buffer (current-buffer))
          (switch-to-buffer scratch-buffer))
      (message "No *scratch* buffer found.")))

  (defun ef-toggle-scratch--goto-prev-buffer ()
    (if (buffer-live-p ef-toggle-scratch--prev-buffer)
        (switch-to-buffer ef-toggle-scratch--prev-buffer)
      (message "No buffer to switch back to.")))

  (defun ef-toggle-scratch ()
    "Toggle between *scratch* buffer and the current buffer."
    (interactive)
    (if (equal (buffer-name) "*scratch*")
        (ef-toggle-scratch--goto-prev-buffer)
      (ef-toggle-scratch--goto-scratch)))

  (define-key evil-normal-state-map ",S" 'ef-toggle-scratch))

(provide 'pkg-scratch)
