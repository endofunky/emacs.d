(use-package lsp-mode
  :commands lsp
  :ensure t
  :custom
  (lsp-auto-guess-root t)
  (lsp-eldoc-enable-hover t)
  (lsp-enable-snippet nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-prefer-flymake :none)
  :config
  (evil-define-key 'normal lsp-mode-map ",," 'lsp-find-definition))

(use-package company-lsp
  :commands company-lsp
  :after (lsp-mode)
  :ensure t
  :config
  (push 'company-lsp company-backends))

(use-package xref
  :defer t
  :after (lsp-mode)
  :custom
  (xref-marker-ring-length 1024)
  :config
  (if (fboundp #'ef-shackle)
      (ef-shackle '("*xref*" :align below :size .4 :popup t :select t)))

  (defadvice xref-goto-xref (after my activate)
    (delete-window (get-buffer-window (get-buffer "*xref*")))))

(provide 'base-lsp)
