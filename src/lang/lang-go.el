(require 'core-lib)
(require 'core-parens)
(require 'core-shackle)

(use-package go-mode
  :ensure t
  :mode (("\\.go\\'" . go-mode))
  :custom
  (gofmt-command "goimports")
  :config
  (sp-with-modes '(go-mode)
    (sp-local-pair "{" nil :post-handlers '((ef-sp-create-newline-and-enter-sexp "RET"))))

  (ef-add-popup "*Gofmt Errors*" :ephemeral t)
  (ef-add-popup "*go-rename*" :ephemeral t)
  (ef-add-popup 'godoc-mode :ephemeral t)

  (add-hook 'before-save-hook #'gofmt-before-save)

  (ef-add-hook go-mode-hook
    (direnv-update-environment)

    (if (locate-file "gopls" exec-path exec-suffixes 1)
        (lsp))

    (if (not (string-match "go" compile-command))
        (set (make-local-variable 'compile-command)
             "go build -v && go vet"))))

(use-package go-rename
  :after go-mode
  :ensure t)

(use-package company-go
  :after go-mode
  :ensure t
  :custom
  (company-go-show-annotation t)
  (company-go-insert-arguments nil)
  :config
  (add-to-list 'company-backends 'company-go))

(use-package go-eldoc
  :after go-mode
  :ensure t
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package gotest
  :after go-mode
  :ensure t
  :custom
  (go-test-verbose t)
  :config
  (ef-add-popup 'go-test-mode :ephemeral t))

(ef-deflang go
  :doc-search godoc
  :test-all go-test-current-project
  :test-file go-test-current-file
  :test-at-point go-test-current-test
  :test-toggle projectile-toggle-between-implementation-and-test)

(provide 'lang-go)
