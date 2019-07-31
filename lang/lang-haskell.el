(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :custom
  (haskell-font-lock-symbols nil)
  :config
  (evil-define-key 'normal haskell-mode-map ",cc" 'haskell-compile))

(use-package haskell-doc
  :after haskell-mode
  :hook (haskell-mode . haskell-doc-mode))

(use-package haskell-indent
  :after haskell-mode
  :hook (haskell-mode . turn-on-haskell-indent))

(provide 'lang-haskell)
