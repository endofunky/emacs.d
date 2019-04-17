(use-package lsp-mode
  :commands lsp
  :ensure t
  :custom
  (lsp-auto-guess-root t)
  (lsp-eldoc-enable-hover nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-prefer-flymake :none)
  :config
  (evil-define-key 'normal lsp-mode-map ",," 'lsp-find-definition)

  (defun ef-lsp-mode-hook ()
    (setq-local company-backends (remove 'company-capf company-backends)))

  (add-hook 'lsp-mode-hook #'ef-lsp-mode-hook))

(use-package company-lsp
  :commands company-lsp
  :after (lsp-mode)
  :ensure t)

(use-package xref
  :defer t
  :after (lsp-mode)
  :config
  (if (fboundp #'ef-shackle)
      (ef-shackle '("*xref*" :align below :size .4 :popup t :select t)))

  (defadvice xref-goto-xref (after my activate)
    (delete-window (get-buffer-window (get-buffer "*xref*")))))

(provide 'pkg-lsp)
