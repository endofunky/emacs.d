(use-package direnv
  :ensure t
  :config
  (direnv-mode)
  (add-hook 'eshell-directory-change-hook #'direnv-update-directory-environment))

(provide 'pkg-direnv)
