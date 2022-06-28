;;; lang-terraform.el --- Terraform configuration -*- lexical-binding: t; -*-
(require 'core-lib)

(use-package terraform-mode
  :mode (("\\.tf\\'" . terraform-mode)))

(provide 'lang-terraform)
