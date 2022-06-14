(require 'core-lib)

(use-package direnv
  :straight t
  :if (executable-find "direnv")
  :config
  (direnv-mode)
  (advice-add 'prog-mode :before #'direnv-update-environment))

(provide 'core-direnv)
