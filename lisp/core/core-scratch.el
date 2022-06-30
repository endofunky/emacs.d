;;; core-scratch.el --- Scratch buffer improvements -*- lexical-binding: t; -*-
(require 'core-evil)

(use-package persistent-scratch
  :defer 1
  :unless noninteractive
  :custom
  (persistent-scratch-autosave-interval 5)
  :hook
  (after-init . persistent-scratch-restore)
  :functions (persistent-scratch-save
              persistent-scratch-setup-default
              persistent-scratch-autosave-mode)
  :defines (persistent-scratch-save-file)
  :config
  (unless (file-exists-p persistent-scratch-save-file)
    (persistent-scratch-save))

  (with-demoted-errors "Error: %S"
    (persistent-scratch-setup-default)
    (persistent-scratch-autosave-mode t)))

(defvar +toggle-scratch--prev-buffer nil)

(defun +toggle-scratch--goto-prev-buffer ()
  (if (buffer-live-p +toggle-scratch--prev-buffer)
      (switch-to-buffer +toggle-scratch--prev-buffer)
    (message "No buffer to switch back to.")))

(defun +toggle-scratch--goto-scratch ()
  (if-let* ((scratch-buffer (get-buffer "*scratch*")))
      (progn
        (setq +toggle-scratch--prev-buffer (current-buffer))
        (switch-to-buffer scratch-buffer))
    (message "No *scratch* buffer found.")))

(defun +toggle-scratch ()
  "Toggle between *scratch* buffer and the current buffer."
  (interactive)
  (if (equal (buffer-name) "*scratch*")
      (+toggle-scratch--goto-prev-buffer)
    (+toggle-scratch--goto-scratch)))

(general-define-key
 :states 'normal
 :prefix ef-leader
 "S" '(+toggle-scratch :wk "Toggle *scratch* buffer"))

;; Never delete the scratch buffer
(+add-hook after-init-hook :fn +init-scratch-timer-h
  (defun +get-or-create-scratch-buffer ()
    "Get *scratch* buffer or create it."
    (unless (get-buffer "*scratch*")
      (with-current-buffer (generate-new-buffer "*scratch*")
        (insert initial-scratch-message)
        (set-buffer-modified-p nil)
        (funcall initial-major-mode))))

  (run-with-idle-timer 1 t '+get-or-create-scratch-buffer))

(provide 'core-scratch)
