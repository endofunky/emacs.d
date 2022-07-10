;;; lang-haskell.el --- Haskell configuration -*- lexical-binding: t; -*-
(require 'core-lib)
(require 'core-evil)

(use-package haskell-mode
  :mode ("\\.hs\\'" "\\.xmobarrc\\'" "xmobarrc\\'")
  :custom
  (haskell-font-lock-symbols nil)
  :general
  (:prefix ef-local-leader :states 'normal :keymaps 'haskell-mode-map
   "c"  '(nil :wk "Compile")
   "cc" '(haskell-compile :wk "Compile")))

(use-package haskell
  :straight nil
  :after haskell-mode)

(use-package haskell-doc
  :straight nil
  :after haskell-mode)

(provide 'lang-haskell)
