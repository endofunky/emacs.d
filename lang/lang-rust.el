(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :config
  (evil-define-key 'normal rust-mode-map ",cr" #'rust-run)
  (evil-define-key 'normal rust-mode-map ",cc" #'rust-compile)
  (evil-define-key 'normal rust-mode-map ",tt" #'rust-test)
  (ef-add-hook rust-mode-hook
    (direnv-update-environment)
    (sp-with-modes '(rust-mode)
      (sp-local-pair "[" nil :post-handlers '((ef-sp-create-newline-and-enter-sexp "RET")))
      (sp-local-pair "{" nil :post-handlers '((ef-sp-create-newline-and-enter-sexp "RET")))
      (sp-local-pair "(" nil :post-handlers '((ef-sp-create-newline-and-enter-sexp "RET"))))
    (lsp)))

(use-package flycheck-rust
  :ensure t
  :after rust-mode
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(provide 'lang-rust)
