(require 'core-projectile)

(use-package terraform-mode
  :straight t
  :mode (("\\.tf\\'" . terraform-mode))
  :hook
  (terraform-mode . ef-terraform-mode-enable-lsp)
  :config
  (defun ef-terraform-mode-enable-lsp ()
    "Conditionally enable lsp-mode for terraform-mode projects."
    (interactive)
    (direnv-update-directory-environment)
    (when (projectile-project-p)
      (when (locate-file "terraform-ls" exec-path)
        (setq-local lsp-terraform-server '("terraform-ls" "serve")))

      (when (locate-file "terraform-lsp" exec-path)
        (setq-local lsp-terraform-server "terraform-lsp"))

      (lsp))))

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
