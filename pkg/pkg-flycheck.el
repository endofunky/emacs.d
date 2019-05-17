(require 'use-package)
(require 'core-lib)
(require 'pkg-shackle)

(use-package flycheck
  :ensure t
  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled))
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
  (defvar ef-flycheck-last-file-buffer nil)

  (defun ef-flycheck-close-window ()
    (when ef-flycheck-may-toggle
      (when-let ((buf (get-buffer flycheck-error-list-buffer)))
        (if-let ((win (get-buffer-window buf)))
            (delete-window win))
        (kill-buffer buf))))

  (defun ef-flycheck-open-window ()
    (if ef-flycheck-may-toggle
        (flycheck-list-errors)))

  (defun ef-flycheck-toggle-window-maybe ()
    (if (and ef-flycheck-may-toggle flycheck-current-errors)
        (ef-flycheck-open-window)
      (ef-flycheck-close-window)))

  (ef-add-hook post-command-hook :fn ef-flycheck-post-command-hook
    (let ((current (current-buffer)))
      (cond ((and (not buffer-file-name)
                  (not (minibufferp current))
                  (not (string= flycheck-error-list-buffer
                                (buffer-name current))))
             (setq ef-flycheck-last-file-buffer nil)
             (ef-flycheck-close-window))
            ((and buffer-file-name
                  (not (minibufferp current))
                  (not (eq ef-flycheck-last-file-buffer
                           current))
                  ef-flycheck-may-toggle)
             (setq ef-flycheck-last-file-buffer current)
             (ef-flycheck-toggle-window-maybe)))))

  (ef-add-hook minibuffer-setup-hook :fn ef-flycheck-minibuffer-setup-hook
    (setq ef-flycheck-may-toggle nil))

  (ef-add-hook minibuffer-exit-hook :fn ef-flycheck-minibuffer-exit-hook
    (setq ef-flycheck-may-toggle t))

  (ef-add-hook flycheck-after-syntax-check-hook
    (ef-flycheck-toggle-window-maybe)))

(use-package pkg-info
  :after flycheck
  :ensure)

(provide 'pkg-flycheck)
