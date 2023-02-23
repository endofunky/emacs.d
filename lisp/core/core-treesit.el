(require 'core-lib)

(use-package treesit)

(use-package treesit-auto
  :demand t
  :custom
  (treesit-auto-install 'prompt)
  :commands
  (global-treesit-auto-mode)
  :config
  (global-treesit-auto-mode))

(provide 'core-treesit)
