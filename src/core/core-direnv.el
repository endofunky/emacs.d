(require 'core-lib)

(use-package direnv
  :if (executable-find "direnv")
  :demand t
  :commands (direnv-mode)
  :config
  (declare-function direnv-update-environment "direnv")
  (direnv-mode)
  (advice-add 'prog-mode :before #'direnv-update-environment))

(provide 'core-direnv)
