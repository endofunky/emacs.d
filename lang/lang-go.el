(use-package go-mode
  :ensure t
  :mode (("\\.go\\'" . go-mode))
  :config
  (evil-define-key 'normal go-mode-map ",," 'godef-jump)
  (evil-define-key 'normal go-mode-map ",." 'pop-tag-mark)

  (sp-with-modes '(go-mode)
    (sp-local-pair "{" nil :post-handlers '((ef-sp-create-newline-and-enter-sexp "RET"))))

  (ef-shackle '("*Gofmt Errors*" :align bottom :size .4 :popup t :select t)
              '("*go-rename*" :align bottom :size .4 :popup t :select t))

  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook #'gofmt-before-save)

  (defun ef-go-mode-hook ()
    (if (not (string-match "go" compile-command))
        (set (make-local-variable 'compile-command)
             "go build -v && go vet")))

  (add-hook 'go-mode-hook 'ef-go-mode-hook))

(use-package go-rename
  :after go-mode
  :ensure t)

(use-package company-go
  :after go-mode
  :ensure t
  :config
  (add-to-list 'company-backends 'company-go)
  (setq company-go-show-annotation t
        company-go-insert-arguments nil))

(use-package go-eldoc
  :after go-mode
  :ensure t
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(provide 'lang-go)
