(require 'core-shackle)

(use-package rust-mode
  :defer t
  :custom
  (rust-format-on-save t)
  :hook
  (rust-mode . ef-enable-lsp-maybe)
  :config
  (ef-add-popup "*rustfmt*"))

(ef-deflang rust
  :compile rust-compile
  :compile-and-run rust-run
  :test-all rust-test)

(provide 'lang-rust)
