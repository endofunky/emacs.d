(use-package go-mode
  :ensure t
  :mode (("\\.go\\'" . go-mode))
  :config
  (evil-define-key 'normal go-mode-map ",," 'godef-jump)
  (evil-define-key 'normal go-mode-map ",." 'pop-tag-mark)
  (setq ac-delay 0.3)
  (sp-with-modes '(go-mode)
    (sp-local-pair "{" nil :post-handlers '((ef-sp-create-newline-and-enter-sexp "RET")))))

(use-package go-rename
  :after go-mode
  :ensure t)

(use-package company-go
  :after go-mode
  :ensure t)

(use-package go-eldoc
  :after go-mode
  :ensure t
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(provide 'lang-go)
