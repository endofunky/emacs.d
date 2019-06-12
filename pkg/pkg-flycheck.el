(require 'pkg-shackle)

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
  :config
  (ef-shackle '("*Flycheck errors*" :align below :size .1 :popup t :no-select t))

  (defun flycheck-may-use-echo-area-p ()
    nil)

  (global-flycheck-mode t)

  (defvar ef-flycheck-may-toggle t)

  (ef-add-hook (post-command-hook
                buffer-list-update-hook
                after-change-major-mode-hook)
    :fn ef-flycheck-post-command-hook
    (when (and ef-flycheck-may-toggle
               (not flycheck-current-errors)
               (not (minibufferp (current-buffer)))
               (get-buffer flycheck-error-list-buffer))
      (delete-windows-on flycheck-error-list-buffer)))

  (ef-add-hook (flycheck-before-syntax-check-hook
                flycheck-after-syntax-check-hook)
    (when flycheck-current-errors
      (flycheck-list-errors)))

  (ef-add-hook minibuffer-setup-hook :fn ef-flycheck-minibuffer-setup-hook
    (setq ef-flycheck-may-toggle nil))

  (ef-add-hook minibuffer-exit-hook :fn ef-flycheck-minibuffer-exit-hook
    (setq ef-flycheck-may-toggle t)))

(use-package pkg-info
  :after flycheck
  :ensure)

(provide 'pkg-flycheck)
