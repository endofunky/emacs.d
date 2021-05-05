(require 'base-shackle)

(use-package flycheck
  :ensure t
  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled idle-buffer-switch))
  (flycheck-buffer-switch-check-intermediate-buffers t)
  (flycheck-idle-buffer-switch-delay 0.01)
  (flycheck-disabled-checkers '(ruby-reek emacs-lisp-checkdoc))
  (flycheck-display-errors-delay 0.1)
  (flycheck-emacs-lisp-initialize-packages 'auto)
  (flycheck-emacs-lisp-load-path load-path)
  (flycheck-error-list-minimum-level nil)
  (flycheck-indication-mode 'left-fringe)
  (flycheck-mode-line-prefix "F")
  (flycheck-navigation-minimum-level 'error)
  (flycheck-syntax-check-buffer)
  :functions (ef-flycheck-toggle-errors)
  :init
  (declare-function flycheck-list-errors "flycheck")
  :config
  (ef-shackle `(,flycheck-error-list-buffer :align below :size .1 :popup t :no-select t))

  (defun flycheck-may-use-echo-area-p ()
    nil)

  (global-flycheck-mode t)

  (defun ef-flycheck-toggle-errors ()
    (interactive)
    (if-let ((win (get-buffer-window flycheck-error-list-buffer)))
        (delete-window win)
      (flycheck-list-errors)))

  (define-key flycheck-error-list-mode-map (kbd "M-e") #'quit-window)
  (define-key flycheck-mode-map (kbd "M-e") #'ef-flycheck-toggle-errors))

(use-package flycheck-color-mode-line
  :after flycheck
  :ensure t
  :commands flycheck-color-mode-line-mode
  :hook (flycheck-mode . flycheck-color-mode-line-mode))

(use-package pkg-info
  :after flycheck
  :ensure)

(provide 'base-flycheck)
