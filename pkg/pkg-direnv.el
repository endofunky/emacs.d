(use-package direnv
  :ensure t
  :config
  (direnv-mode)
  (ef-add-hook eshell-directory-change-hook
    (direnv-update-directory-environment)
    (setq eshell-path-env (getenv "PATH"))))

(provide 'pkg-direnv)
