(use-package coffee-mode
  :ensure t
  :mode ("\\.coffee\\'" "Cakefile\\'")
  :config
  (defun ef-coffee-mode-hook ()
    (sp-with-modes '(coffee-mode)
      (sp-local-pair "[" nil :post-handlers '((ef-sp-create-newline-and-enter-sexp "RET")))
      (sp-local-pair "{" nil :post-handlers '((ef-sp-create-newline-and-enter-sexp "RET")))
      (sp-local-pair "(" nil :post-handlers '((ef-sp-create-newline-and-enter-sexp "RET"))))
    (setq coffee-tab-width 2))

  (add-hook 'coffee-mode-hook 'ef-coffee-mode-hook)

  (define-key evil-visual-state-map ">" 'coffee-indent-shift-right)
  (define-key evil-normal-state-map ">" 'coffee-indent-shift-right)
  (define-key evil-visual-state-map "<" 'coffee-indent-shift-left)
  (define-key evil-normal-state-map "<" 'coffee-indent-shift-left))

(provide 'lang-coffee)
