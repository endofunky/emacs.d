(use-package rust-mode
  :ensure t
  :hook (rust-mode . lsp)
  :mode "\\.rs\\'"
  :config
  (evil-define-key 'normal rust-mode-map ",cr" #'rust-run)
  (evil-define-key 'normal rust-mode-map ",cc" #'rust-compile)
  (evil-define-key 'normal rust-mode-map ",tt" #'rust-test))

(use-package flycheck-rust
  :ensure t
  :after rust-mode
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(provide 'lang-rust)
