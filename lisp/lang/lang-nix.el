;;; lang-nix.el --- Nix configuration -*- lexical-binding: t; -*-
(require 'core-evil)

(use-package nix-mode
  :mode ("\\.nix\\'" "\\.nix.in\\'")
  :general
  (:prefix ef-local-leader :states 'normal :keymaps 'nix-mode-map
   "TAB" '(nix-mode-format :wk "Format buffer")

   "c"  '(nil :wk "Nix")
   "cu" '(nix-update-fetch "update-fetch")

   "r"  '(nil :wk "REPL")
   "rr" '(nix-repl :wk "Open"))
  :config
  (poe-popup "*Nix-REPL*"))

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

(use-package nix-flymake
  :after nix-mode
  :straight (:repo "tviti/nix-flymake"
             :host github)
  :hook
  (nix-mode . +nix-flymake-setup-h)
  :config
  (defun +nix-flymake-setup-h ()
    "Enable flymake backend."
    (interactive)
    (add-hook 'flymake-diagnostic-functions #'nix-flymake nil t)))

(provide 'lang-nix)
