(require 'core-evil)
(require 'core-lib)
(require 'core-shackle)

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
  (ef-add-popup "*Nix-REPL*")
  (evil-define-key 'normal nix-mode-map ",r" 'nix-repl))

(use-package nix-update
  :ensure t
  :after nix-mode
  :commands nix-update-fetch
  :init
  (evil-define-key 'normal nix-mode-map ",cu" #'nix-update-fetch))

(provide 'lang-nix)
