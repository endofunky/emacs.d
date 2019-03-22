(use-package lsp-mode
  :commands lsp
  :ensure t
  :custom
  (lsp-prefer-flymake :none)
  (lsp-auto-guess-root t)
  (lsp-inhibit-message t)
  :config
  (evil-define-key 'normal lsp-mode-map ",," 'lsp-find-definition)
  (evil-define-key 'normal lsp-mode-map ",." 'pop-tag-mark))

(use-package company-lsp
  :commands company-lsp
  :after lsp-mode
  :ensure t)

(provide 'pkg-lsp)
