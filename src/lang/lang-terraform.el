(require 'core-lib)

(use-package terraform-mode
  :mode (("\\.tf\\'" . terraform-mode))
  :hook
  (terraform-mode . ef-enable-lsp-maybe))

(provide 'lang-terraform)
