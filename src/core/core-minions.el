(use-package minions
  :ensure t
  :custom
  (minions-direct '(flycheck-mode
                    projectile-mode))
  :config
  (minions-mode))

(provide 'core-minions)
