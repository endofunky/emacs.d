(use-package flycheck
  :ensure t
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc
                                             ruby-rubocop
                                             ruby-rubylint
                                             ruby-reek))

  (global-flycheck-mode t))

(use-package flycheck-popup-tip
  :after flycheck
  :ensure t
  :config
  (flycheck-popup-tip-mode t))

(provide 'pkg-flycheck)

