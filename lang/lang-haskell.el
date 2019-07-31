(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :custom
  (haskell-font-lock-symbols nil)
  :config
  (evil-define-key 'normal haskell-mode-map ",cc" 'haskell-compile))

(use-package haskell
  :after haskell-mode)

(use-package haskell-doc
  :after haskell-mode
  :hook (haskell-mode . haskell-doc-mode))

(use-package intero
  :ensure t
  :commands intero-mode
  :init
  (add-hook 'haskell-mode-hook #'intero-mode)
  :config
  (ef-shackle '(intero-repl-mode :align below :size .4 :popup t :select t))

  (evil-define-key 'visual intero-mode-map ",er" 'intero-repl-eval-region)
  (evil-define-key 'normal intero-mode-map ",ef" 'intero-repl-load)
  (evil-define-key 'normal intero-mode-map ",r" 'intero-repl)
  (evil-define-key 'normal intero-mode-map ",," 'intero-goto-definition)
  (evil-define-key 'normal intero-repl-mode-map ",r" 'quit-window))

(provide 'lang-haskell)
