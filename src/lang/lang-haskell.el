(use-package haskell-mode
  :straight t
  :mode "\\.hs\\'"
  :custom
  (haskell-font-lock-symbols nil)
  :config
  (evil-define-key 'normal haskell-mode-map ",cc" 'haskell-compile))

(use-package haskell
  :after haskell-mode)

(use-package haskell-doc
  :after haskell-mode)

(provide 'lang-haskell)
