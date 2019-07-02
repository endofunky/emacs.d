(use-package nix-mode
  :ensure t
  :mode ("\\.nix\\'" "\\.nix.in\\'")
  :config
  (evil-define-key 'normal nix-mode-map (kbd ", TAB") 'nix-mode-format))

(use-package nix-drv-mode
  :ensure nix-mode
  :mode "\\.drv\\'")

(use-package nix-shell
  :ensure nix-mode
  :commands (nix-shell-unpack nix-shell-configure nix-shell-build))

(use-package nix-repl
  :ensure nix-mode
  :commands (nix-repl)
  :config
  (ef-define-repl ef-repl-nix "*Nix-REPL*" #'nix-repl)
  (evil-define-key 'normal nix-mode-map ",r" 'ef-repl-nix)
  (evil-define-key 'normal nix-repl-mode-map ",r" 'ef-repl-nix))

(provide 'lang-nix)
