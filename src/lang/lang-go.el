(require 'core-shackle)

(use-package go-mode
  :mode (("\\.go\\'" . go-mode))
  :custom
  (lsp-go-hover-kind "NoDocumentation")
  :hook
  (go-mode . lsp)
  :functions (gofmt-before-save)
  :config
  (declare-function ef-lsp-organize-imports "core-lsp")

  (ef-add-popup "*Gofmt Errors*" :ephemeral t)
  (ef-add-popup "*go-rename*" :ephemeral t)
  (ef-add-popup 'godoc-mode :ephemeral t)

  (defconst ef-go-test-toggle-re "_test\\."
    "Match regexp for `ef-go-test-toggle'.")

  (defun ef-go-test-toggle ()
    "Toggle Go test/implementation."
    (interactive)
    (let ((file (buffer-file-name (current-buffer))))
      (if (string-match ef-go-test-toggle-re file)
          (let ((impl (replace-regexp-in-string ef-go-test-toggle-re "." file)))
            (if (file-exists-p impl)
                (find-file impl)
              (message "Implementation not found: %s" impl)))
        (let ((test-file (concat (file-name-directory file)
                                 (file-name-base file)
                                 "_test."
                                 (file-name-extension file))))
          (if (file-exists-p test-file)
              (find-file test-file)
            (message "Test file not found: %s" test-file))))))

  (ef-add-hook go-mode-hook
    (add-hook 'before-save-hook #'gofmt-before-save nil t)
    (add-hook 'before-save-hook #'ef-lsp-organize-imports nil t)

    (if (not (string-match "go" compile-command))
        (set (make-local-variable 'compile-command)
             "go build -v && go vet"))))

(use-package gotest
  :after go-mode
  :custom
  (go-test-verbose t)
  :config
  (ef-add-popup 'go-test-mode :ephemeral t))

(ef-deflang go
  :doc-search godoc
  :test-all go-test-current-project
  :test-at-point go-test-current-test
  :test-file go-test-current-file
  :test-toggle ef-go-test-toggle)

(provide 'lang-go)
