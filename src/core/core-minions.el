(use-package minions
  :straight t
  :custom
  (minions-direct '(flycheck-mode
                    lsp-mode
                    projectile-mode))
  :config
  (minions-mode))

(provide 'core-minions)
