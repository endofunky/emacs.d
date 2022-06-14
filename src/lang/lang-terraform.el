(require 'core-projectile)

(use-package terraform-mode
  :straight t
  :mode (("\\.tf\\'" . terraform-mode))
  :hook
  (terraform-mode . lsp))

(use-package lsp-mode
  :defer t
  :config
  (declare-function lsp-flycheck-add-mode "lsp-mode")

  (lsp-flycheck-add-mode 'terraform-mode))

(use-package company-terraform
  :straight t
  :after terraform-mode
  :config
  (company-terraform-init))

(provide 'lang-terraform)
