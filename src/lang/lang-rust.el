(require 'core-shackle)

(use-package rustic
  :custom
  (rustic-lsp-server 'rust-analyzer)
  (rustic-lsp-client 'eglot)
  (rustic-format-on-save t)
  (rust-prettify-symbols-alist nil)
  :config
  (ef-add-popup "*rustic-compilation*" :ephemeral t)
  (ef-add-popup "*cargo-test*" :ephemeral t)

  ;; Don't ask to install LSP server if it's not installed.
  (advice-add 'rustic-install-lsp-client-p :override #'ignore))

(ef-deflang rust
  :compile rustic-cargo-build
  :test-all rustic-cargo-test
  :test-file rustic-cargo-current-test)

(provide 'lang-rust)
