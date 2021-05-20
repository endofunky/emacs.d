(require 'core-lib)

(use-package direnv
  :ensure t
  :if (executable-find "direnv")
  :config
  (direnv-mode)
  (defadvice lsp (before ef-direnv activate)
    (direnv-update-environment)))

(provide 'core-direnv)
