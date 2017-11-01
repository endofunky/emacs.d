(use-package cc-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.h$" . c-mode))
  :config
  (setq c-default-style "k&r")
  (setq c-basic-offset 8)
  (setq tab-width 8)
  (setq indent-tabs-mode t)

  (defun ef-c-mode-hook ()
    (sp-with-modes '(c-mode cc-mode)
      (sp-local-pair "#include <" ">")
      (sp-local-pair "[" nil :post-handlers '((ef-sp-create-newline-and-enter-sexp "RET")))
      (sp-local-pair "{" nil :post-handlers '((ef-sp-create-newline-and-enter-sexp "RET")))
      (sp-local-pair "(" nil :post-handlers '((ef-sp-create-newline-and-enter-sexp "RET")))))

  (add-hook 'c-mode-hook 'ef-c-mode-hook))

(use-package company-c-headers
  :after cc-mode
  :ensure t
  :config
  (add-to-list 'company-backends 'company-c-headers))

(use-package cmake-mode
  :after cc-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

(provide 'lang-c)
