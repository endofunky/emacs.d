(use-package flycheck
  :ensure t
  :config
  (use-package flycheck-popup-tip
    :ensure t
    :config
    (flycheck-popup-tip-mode t))

  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc
                                             ruby-rubocop
                                             ruby-rubylint
                                             ruby-reek))

  (global-flycheck-mode t))

(provide 'pkg-flycheck)

