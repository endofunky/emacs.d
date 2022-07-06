;;; lang-nix.el --- Nix configuration -*- lexical-binding: t; -*-
(require 'core-evil)
(require 'core-tree-sitter)

(use-package nix-mode
  :mode ("\\.nix\\'" "\\.nix.in\\'")
  :general
  (:prefix ef-local-leader :states 'normal :keymaps 'nix-mode-map
   "c"  '(nil :wk "Nix")
   "cu" '(nix-update-fetch "update-fetch")

   "r"  '(nil :wk "REPL")
   "rr" '(nix-repl :wk "Open"))
  :config
  (+enable-tree-sitter nix-mode)
  (evil-define-key 'normal nix-mode-map (kbd ", TAB") 'nix-mode-format))

(use-package nix-drv-mode
  :straight nix-mode
  :mode "\\.drv\\'")

(use-package nix-shell
  :straight nix-mode
  :commands (nix-shell-unpack nix-shell-configure nix-shell-build))

(use-package nix-repl
  :straight nix-mode
  :commands (nix-repl))

(use-package nix-update
  :after nix-mode
  :commands nix-update-fetch)

(provide 'lang-nix)
