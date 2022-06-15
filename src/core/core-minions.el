(require 'core-lib)

(use-package minions
  :demand t
  :commands (minions-mode)
  :custom
  (minions-direct '(lsp-mode
                    projectile-mode))
  :config
  (minions-mode))

(provide 'core-minions)
