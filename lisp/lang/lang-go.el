;;; lang-go.el --- Go configuration -*- lexical-binding: t; -*-
(require 'core-lsp)
(require 'core-shackle)
(require 'core-tree-sitter)

(use-package go-mode
  :mode (("\\.go\\'" . go-mode))
  :functions (gofmt-before-save)
  :config
  (ef-lsp go-mode)
  (ef-tree-sitter go-mode)

  (declare-function ef-lsp-organize-imports "core-lsp")

  (ef-add-popup "*Gofmt Errors*" :ephemeral t)
  (ef-add-popup "*go-rename*" :ephemeral t)
  (ef-add-popup 'godoc-mode :ephemeral t)

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
  :general
  (:prefix ef-local-leader :states 'normal :keymaps 'go-mode-map
   "t"  '(nil :wk "Test")
   "ta" '(go-test-current-project :wk "Project")
   "tt" '(go-test-current-file :wk "Buffer")
   "tp" '(go-test-current-test :wk "At point")
   "tl" '(ef-go-test-toggle :wk "Toggle"))
  :config
  (ef-add-popup 'go-test-mode :ephemeral t)

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
            (message "Test file not found: %s" test-file)))))))

(provide 'lang-go)
