(require 'core-shackle)

(use-package envrc
  :if (executable-find "direnv")
  :demand t
  :commands (envrc-global-mode)
  :general
  (:states 'normal :prefix ef-prefix
   "E"  '(envrc-command-map :wk "Envrc"))
  :config
  (ef-add-popup "*envrc*" :ephemeral t)
  (envrc-global-mode))

(provide 'core-direnv)
