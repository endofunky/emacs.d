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

(use-package nixos-options
  :general
  (:keymaps 'nix-mode-map :states 'normal
   "K" '+nix/lookup-option)
  :functions (nixos-options-doc-buffer
              nixos-options-get-documentation-for-option
              +nix--options-action)
  :defines (nixos-options)
  :config
  (defun +nix--options-action (candidate)
    (switch-to-buffer-other-window
     (nixos-options-doc-buffer
      (nixos-options-get-documentation-for-option candidate))))

  (defun +nix/lookup-option (&optional initial-input)
    "Look up documentation on a nix option."
    (interactive
     (list
      ;; REVIEW Must be a better way to do this
      (when (looking-at-p "[a-zA-Z0-9-_\\.]")
        (buffer-substring-no-properties
         (save-excursion
           (skip-chars-backward "^ ")
           (point))
         (save-excursion
           (skip-chars-forward "^ ")
           (point))))))
    (+nix--options-action (cdr
                           (assoc
                            (completing-read "NixOs options: "
                                             nixos-options
                                             nil
                                             t
                                             initial-input) nixos-options)))
    ;; Tell lookup module to let us handle things from here
    'deferred))


(provide 'lang-nix)
