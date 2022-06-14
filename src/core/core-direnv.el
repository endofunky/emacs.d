(require 'core-lib)

(use-package direnv
  :if (executable-find "direnv")
  :config
  (direnv-mode)
  (advice-add 'prog-mode :before #'direnv-update-environment))

(provide 'core-direnv)
