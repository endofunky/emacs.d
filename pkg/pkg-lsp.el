(use-package lsp-mode
  :commands lsp
  :ensure t
  :custom
  (lsp-prefer-flymake :none)
  (lsp-auto-guess-root t)
  (lsp-inhibit-message t)
  ;; eldoc bug??
  (lsp-eldoc-enable-hover nil)
  (lsp-eldoc-enable-signature-help nil)
  (lsp-eldoc-prefer-signature-help nil)
  (lsp-enable-symbol-highlighting nil)
  :config
  (evil-define-key 'normal lsp-mode-map ",," 'lsp-find-definition)
  (evil-define-key 'normal lsp-mode-map ",." 'pop-tag-mark)

  (defun ef-lsp-mode-hook ()
    (setq-local company-backends (remove 'company-capf company-backends)))

  (add-hook 'lsp-mode-hook 'ef-lisp-mode-hook))

(use-package company-lsp
  :commands company-lsp
  :ensure t)

(provide 'pkg-lsp)
