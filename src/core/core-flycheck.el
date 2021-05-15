(require 'core-shackle)
(require 'core-evil)

(use-package flycheck
  :ensure t
  :demand t
  :custom
  (flycheck-buffer-switch-check-intermediate-buffers t)
  (flycheck-idle-buffer-switch-delay 0.01)
  (flycheck-idle-change-delay 0.1)
  (flycheck-disabled-checkers '(ruby-reek emacs-lisp-checkdoc))
  (flycheck-display-errors-delay 0.1)
  (flycheck-emacs-lisp-initialize-packages 'auto)
  (flycheck-emacs-lisp-load-path load-path)
  (flycheck-error-list-minimum-level nil)
  (flycheck-indication-mode 'left-fringe)
  (flycheck-mode-line-prefix "F")
  (flycheck-navigation-minimum-level 'error)
  (flycheck-syntax-check-buffer)
  :functions (ef-flycheck-toggle-errors
              ef-flycheck-buffer-maybe)
  :hook
  (evil-insert-state-exit . ef-flycheck-check-buffer-maybe)
  (evil-replace-state-exit . ef-flycheck-check-buffer-maybe)
  :general
  (:keymap 'flycheck-error-list-mode-map
	   "M-e" 'quit-window)
  (:keymap 'flycheck--mode-map
	   "M-e" 'ef-flycheck-toggle-errors)
  :config
  (declare-function flycheck-buffer "flycheck")
  (declare-function flycheck-list-errors "flycheck")

  (defadvice flycheck-handle-change (around magit-fullscreen activate)
    (unless (or (eq evil-state 'insert)
                (eq evil-state 'replace))
      ad-do-it))

  (ef-shackle `(,flycheck-error-list-buffer :align below :size .1 :popup t :no-select t :popup-float t))

  (fset 'flycheck-may-use-echo-area-p 'ignore)

  (global-flycheck-mode t)

  (defun ef-flycheck-check-buffer-maybe ()
    (when (bound-and-true-p flycheck-mode)
      (flycheck-buffer)))

  (defun ef-flycheck-toggle-errors ()
    (interactive)
    (if-let ((win (get-buffer-window flycheck-error-list-buffer)))
        (delete-window win)
      (flycheck-list-errors))))

(use-package flycheck-color-mode-line
  :after flycheck
  :ensure t
  :commands flycheck-color-mode-line-mode
  :hook (flycheck-mode . flycheck-color-mode-line-mode))

(use-package pkg-info
  :after flycheck
  :ensure t)

(provide 'core-flycheck)
