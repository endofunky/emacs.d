(require 'core-shackle)

(use-package rustic
  :custom
  (rustic-lsp-server 'rust-analyzer)
  (rustic-lsp-client 'eglot)
  (rustic-format-on-save t)
  (rust-prettify-symbols-alist nil)
  :config
  (ef-add-popup "*rustfmt*" :ephemeral t)
  (ef-add-popup "*rustic-compilation*" :ephemeral t)
  (ef-add-popup "*cargo-run*" :ephemeral t)
  (ef-add-popup "*cargo-run-comint*" :ephemeral t)
  (ef-add-popup "*cargo-test*" :ephemeral t)

  (ef-add-hook rustic-mode-hook
    (setq-local mode-name "Rust"))

  ;; Don't ask to install LSP server if it's not installed.
  (advice-add 'rustic-install-lsp-client-p :override #'ignore))

(ef-deflang rust
  :compile rustic-cargo-build
  :compile-and-run rustic-cargo-run
  :test-all rustic-cargo-test
  :test-file rustic-cargo-current-test)

(provide 'lang-rust)
