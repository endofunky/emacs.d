(require 'core-lib)

(use-package direnv
  :ensure t
  :config
  (direnv-mode)
  (defadvice lsp (before ef-direnv activate)
    (direnv-update-environment)))

(provide 'base-direnv)
