(require 'core-lib)

(use-package envrc
  :if (executable-find "direnv")
  :demand t
  :commands (envrc-global-mode)
  :config
  (envrc-global-mode))


(provide 'core-direnv)
