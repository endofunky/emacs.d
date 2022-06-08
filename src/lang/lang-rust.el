(require 'core-lib)
(require 'core-shackle)

(use-package rust-mode
  :custom
  (rust-format-on-save t)
  :straight t
  :config
  (ef-add-hook rust-mode-hook :interactive t
    (direnv-update-directory-environment)

    (if (locate-file "rls" exec-path exec-suffixes 1)
        (lsp)))

  (ef-add-popup "*rustfmt*"))

(ef-deflang rust
  :compile rust-compile
  :compile-and-run rust-run
  :test-all rust-test)

(provide 'lang-rust)
