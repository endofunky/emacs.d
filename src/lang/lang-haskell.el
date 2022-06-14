(require 'core-lib)
(require 'core-evil)

(use-package haskell-mode
  :mode "\\.hs\\'"
  :custom
  (haskell-font-lock-symbols nil)
  :config
  (evil-define-key 'normal haskell-mode-map ",cc" 'haskell-compile))

(use-package haskell
  :straight nil
  :after haskell-mode)

(use-package haskell-doc
  :straight nil
  :after haskell-mode)

(provide 'lang-haskell)
