;;; lang-haskell.el --- Haskell configuration -*- lexical-binding: t; -*-
(require 'core-lib)
(require 'core-evil)
(require 'core-tree-sitter)

(use-package haskell-mode
  :mode ("\\.hs\\'" "\\.xmobarrc\\'" "xmobarrc\\'")
  :custom
  (haskell-font-lock-symbols nil)
  :config
  (+enable-tree-sitter haskell-mode)
  (evil-define-key 'normal haskell-mode-map ",cc" 'haskell-compile))

(use-package haskell
  :straight nil
  :after haskell-mode)

(use-package haskell-doc
  :straight nil
  :after haskell-mode)

(provide 'lang-haskell)
