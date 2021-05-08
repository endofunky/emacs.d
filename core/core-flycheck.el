(require 'core-shackle)
(require 'core-evil)

(use-package flycheck
  :ensure t
  :demand t
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
  :general
  (:keymap 'flycheck-error-list-mode-map
	   "M-e" 'quit-window)
  (:keymap 'flycheck--mode-map
	   "M-e" 'ef-flycheck-toggle-errors)
  :config
  (declare-function flycheck-buffer "flycheck")
  (declare-function flycheck-list-errors "flycheck")

  (ef-shackle `(,flycheck-error-list-buffer :align below :size .1 :popup t :no-select t))

  (fset 'flycheck-may-use-echo-area-p 'ignore)

  (global-flycheck-mode t)

  (defun ef-flycheck-toggle-errors ()
    (interactive)
    (if-let ((win (get-buffer-window flycheck-error-list-buffer)))
        (delete-window win)
      (flycheck-list-errors)))

  (defconst ef-flycheck-need-update-commands
    '(evil-change
      evil-delete
      evil-insert
      evil-normal-state
      evil-paste-after
      evil-paste-before
      evil-replace
      evil-undo
      evilnc-comment-or-uncomment-lines
      lispyville-comment-or-uncomment-line
      lispyville-comment-or-uncomment
      lispyville-delete))

  (ef-add-hook post-command-hook :fn ef-flycheck-post-command-hook
    (when (and (bound-and-true-p flycheck-mode)
               (member this-command ef-flycheck-need-update-commands))
      (flycheck-buffer))))

(use-package flycheck-color-mode-line
  :after flycheck
  :ensure t
  :commands flycheck-color-mode-line-mode
  :hook (flycheck-mode . flycheck-color-mode-line-mode))

(use-package pkg-info
  :after flycheck
  :ensure t)

(provide 'core-flycheck)
