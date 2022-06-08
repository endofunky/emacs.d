(require 'core-lib)

(use-package direnv
  :straight t
  :if (executable-find "direnv")
  :config
  (direnv-mode)
  (defadvice lsp (before ef-lsp-direnv activate)
    (direnv-update-environment)))

(provide 'core-direnv)
