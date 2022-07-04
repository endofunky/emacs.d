;;; core-direnv.el --- Direnv environments -*- lexical-binding: t; -*-
(require 'core-shackle)

(use-package envrc
  :if (executable-find "direnv")
  :demand t
  :commands (envrc-global-mode)
  :config
  (poe-popup "*envrc*" :ephemeral t)
  (envrc-global-mode))

(provide 'core-direnv)
