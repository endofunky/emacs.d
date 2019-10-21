(use-package direnv
  :ensure t
  :config
  (direnv-mode)
  (defadvice lsp (before ef-direnv activate)
    (direnv-update-environment))

  (ef-add-hook eshell-directory-change-hook
    (direnv-update-directory-environment)
    (setq eshell-path-env (getenv "PATH"))))

(provide 'base-direnv)
