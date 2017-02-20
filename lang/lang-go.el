(use-package go-mode
  :ensure t
  :mode (("\\.go\\'" . go-mode))
  :config
  (use-package go-autocomplete
    :ensure t)
  (use-package go-rename
    :ensure t)
  (use-package go-eldoc
    :ensure t
    :init
    (add-hook 'go-mode-hook 'go-eldoc-setup))
  (evil-define-key 'normal go-mode-map ",," 'godef-jump)
  (evil-define-key 'normal go-mode-map ",." 'pop-tag-mark)
  (setq ac-delay 0.3)
  (sp-with-modes '(go-mode)
    (sp-local-pair "{" nil :post-handlers '((ts/sp-create-newline-and-enter-sexp "RET")))))

(provide 'lang-go)
