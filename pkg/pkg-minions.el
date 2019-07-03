(use-package minions
  :ensure t
  :custom
  (minions-direct '(flycheck-mode
                    lsp-mode
                    projectile-mode))
  :config
  (minions-mode))

(provide 'pkg-minions)
