(require 'core-lib)

(use-package minions
  :demand t
  :commands (minions-mode)
  :custom
  (minions-direct '(flycheck-mode
                    lsp-mode
                    projectile-mode))
  :config
  (minions-mode))

(provide 'core-minions)
