;;; lang-rust.el --- Rust configuration -*- lexical-binding: t; -*-
(require 'core-eglot)
(require 'core-popup)

(use-package rust-ts-mode
  :straight nil
  :mode ("\\.rs\\'" . rust-ts-mode)
  :general
  (:prefix ef-local-leader :states 'normal :keymaps 'rust-ts-mode-map
   "c"  '(nil :wk "Cargo")
   "cc" '(rust-compile :wk "Build")
   "cr" '(rust-run :wk "Run")

   "l"  '(nil :wk "Lint")
   "ly" '(rust-run-clippy :wk "Clippy")

   "lf"  '(nil :wk "Format")
   "lfb" '(rust-format-buffer :wk "Buffer")
   "lfd" '(rust-format-diff-buffer :wk "Diff")

   "t"  '(nil :wk "Test")
   "ta" '(rust-test :wk "All"))
  :config
  (require 'rust-rustfmt)
  (+enable-lsp rust-ts-mode))

(use-package rust-rustfmt
  :after rust-ts-mode
  :straight (rust-rustfmt :type git
                          :host github
                          :repo "rust-lang/rust-mode"
                          :branch "master"
                          :files ("rust-rustfmt.el"))
  :commands (rust-format-buffer
             rust-format-diff-buffer)
  :general
  (:prefix ef-local-leader :states 'normal :keymaps 'rust-ts-mode-map
   "l"  '(nil :wk "Lint")
   "lf"  '(nil :wk "Format")
   "lfb" '(rust-format-buffer :wk "Buffer")
   "lfd" '(rust-format-diff-buffer :wk "Diff"))
  :config
  (+add-hook rust-ts-mode-hook
    (add-hook 'before-save-hook 'rust-format-buffer nil t)))


(provide 'lang-rust)
