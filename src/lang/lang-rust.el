(require 'core-shackle)

(use-package rustic
  :custom
  (rustic-lsp-server 'rust-analyzer)
  (rustic-lsp-client 'eglot)
  (rustic-lsp-setup-p nil)
  (rustic-format-on-save t)
  (rust-prettify-symbols-alist nil)
  :hook
  (rustic-mode . ef-enable-lsp-maybe)
  :functions (ef-update-cargo-bin)
  :defines (rustic-cargo-bin)
  :config
  (ef-add-popup "^\\*rust" :ephemeral t :regexp t)
  (ef-add-popup "^\\*cargo" :ephemeral t :regexp t)

  (ef-add-hook rustic-mode-hook
    (setq-local mode-name "Rust"))

  ;; Set `rustic-cargo-bin' globally in "real-time" so rustic sees get the
  ;; correct one from PATH set via `envrc'.
  ;;
  ;; We can't `setq-local' this in a major-mode hook as rustic configures it's
  ;; workspace wrapped in `with-temp-buffer' which has a different environment.
  (defun ef-update-cargo-bin (&rest _)
    ;; Don't update the path when we're in the temp-buffer, which has
    ;; `fundamental-mode' as it's major-mode.
    (unless (eq major-mode 'fundamental-mode)
      (setq rustic-cargo-bin (or (executable-find "cargo")
                                 "cargo"))))

  (advice-add 'rustic-cargo-bin :before #'ef-update-cargo-bin)

  ;; Don't ask to install LSP server if it's not installed.
  (advice-add 'rustic-install-lsp-client-p :override #'ignore))

(ef-deflang rust
  :compile rustic-cargo-build
  :compile-and-run rustic-cargo-run
  :test-all rustic-cargo-test
  :test-file rustic-cargo-current-test)

(provide 'lang-rust)
