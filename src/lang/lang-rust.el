(require 'core-lib)
(require 'core-shackle)

(use-package rust-mode
  :defer t
  :custom
  (rust-format-on-save t)
  :straight t
  :hook
  (rust-mode . lsp)
  :config
  (ef-add-popup "*rustfmt*"))

(ef-deflang rust
  :compile rust-compile
  :compile-and-run rust-run
  :test-all rust-test)

(provide 'lang-rust)
