(require 'core-lib)

(use-package terraform-mode
  :mode (("\\.tf\\'" . terraform-mode))
  :hook
  (terraform-mode . ef-enable-lsp-maybe))

(use-package company-terraform
  :after terraform-mode
  :functions (company-terraform-init)
  :config
  (company-terraform-init))

(provide 'lang-terraform)
