(use-package cc-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.h$" . c-mode))
  :config
  (setq c-default-style "k&r")
  (setq c-basic-offset 8)
  (setq tab-width 8)
  (setq indent-tabs-mode t)

  (defun ts/c-mode-hook ()
    (sp-with-modes '(c-mode cc-mode)
      (sp-local-pair "#include <" ">")
      (sp-local-pair "[" nil :post-handlers '((ts/sp-create-newline-and-enter-sexp "RET")))
      (sp-local-pair "{" nil :post-handlers '((ts/sp-create-newline-and-enter-sexp "RET")))
      (sp-local-pair "(" nil :post-handlers '((ts/sp-create-newline-and-enter-sexp "RET")))))

  (add-hook 'c-mode-hook 'ts/c-mode-hook)

  (use-package ac-c-headers
    :ensure t
    :init
    (add-hook 'c-mode-hook
              (lambda ()
                (add-to-list 'ac-sources 'ac-source-c-headers)
                (add-to-list 'ac-sources 'ac-source-c-header-symbols t)))))

(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

(provide 'lang-c)
