(require 'core-evil)

(use-package persistent-scratch
  :ensure t
  :defer 1
  :unless noninteractive
  :custom
  (persistent-scratch-autosave-interval 5)
  :hook
  (after-init . persistent-scratch-restore)
  :config
  (persistent-scratch-setup-default)
  (persistent-scratch-autosave-mode t))

(defvar ef-toggle-scratch--prev-buffer nil)

(defun ef-toggle-scratch--goto-prev-buffer ()
  (if (buffer-live-p ef-toggle-scratch--prev-buffer)
      (switch-to-buffer ef-toggle-scratch--prev-buffer)
    (message "No buffer to switch back to.")))

(defun ef-toggle-scratch--goto-scratch ()
  (if-let* ((scratch-buffer (get-buffer "*scratch*")))
      (progn
        (setq ef-toggle-scratch--prev-buffer (current-buffer))
        (switch-to-buffer scratch-buffer))
    (message "No *scratch* buffer found.")))

(defun ef-toggle-scratch ()
  "Toggle between *scratch* buffer and the current buffer."
  (interactive)
  (if (equal (buffer-name) "*scratch*")
      (ef-toggle-scratch--goto-prev-buffer)
    (ef-toggle-scratch--goto-scratch)))

(general-define-key
 :states 'normal
 :prefix ef-prefix
 "S" 'ef-toggle-scratch)

;; Never delete the scratch buffer
(ef-add-hook after-init-hook :fn ef-init-scratch-timer-hook
  (defun ef-get-scratch-buffer-create ()
    "Get *scratch* buffer or create it."
    (unless (get-buffer "*scratch*")
      (with-current-buffer (generate-new-buffer "*scratch*")
        (insert initial-scratch-message)
        (set-buffer-modified-p nil)
        (funcall initial-major-mode))))

  (run-with-idle-timer 1 t 'ef-get-scratch-buffer-create))

(provide 'core-scratch)
