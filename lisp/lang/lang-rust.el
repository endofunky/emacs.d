;;; lang-rust.el --- Rust configuration -*- lexical-binding: t; -*-
(require 'core-lsp)
(require 'core-shackle)
(require 'core-tree-sitter)

(use-package rustic
  :custom
  (rustic-lsp-server 'rust-analyzer)
  (rustic-lsp-client 'eglot)
  (rustic-lsp-setup-p nil)
  (rustic-format-on-save t)
  (rust-prettify-symbols-alist nil)
  :functions (+update-cargo-bin)
  :defines (rustic-cargo-bin)
  :general
  (:prefix ef-local-leader :states 'normal :keymaps 'rustic-mode-map
   "c"  '(nil :wk "Compile")
   "cc" '(rustic-cargo-build :wk "Build")
   "cr" '(rustic-cargo-run :wk "Run")

   "t"  '(nil :wk "Test")
   "ta" '(rustic-cargo-test :wk "At point")
   "tp" '(rustic-cargo-current-test :wk "At point"))
  :config
  (+enable-lsp rustic-mode)
  (+enable-tree-sitter rustic-mode)

  (+add-popup "^\\*rust" :ephemeral t :regexp t)
  (+add-popup "^\\*cargo" :ephemeral t :regexp t)

  (+add-hook rustic-mode-hook
    (setq-local mode-name "Rust"))

  ;; Set `rustic-cargo-bin' globally in "real-time" so rustic sees get the
  ;; correct one from PATH set via `envrc'.
  ;;
  ;; We can't `setq-local' this in a major-mode hook as rustic configures it's
  ;; workspace wrapped in `with-temp-buffer' which has a different environment.
  (defun +update-cargo-bin (&rest _)
    ;; Don't update the path when we're in the temp-buffer, which has
    ;; `fundamental-mode' as it's major-mode.
    (unless (eq major-mode 'fundamental-mode)
      (setq rustic-cargo-bin (or (executable-find "cargo")
                                 "cargo"))))

  (advice-add 'rustic-cargo-bin :before #'+update-cargo-bin)

  ;; Don't ask to install LSP server if it's not installed.
  (advice-add 'rustic-install-lsp-client-p :override #'ignore))

(provide 'lang-rust)
