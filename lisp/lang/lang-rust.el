;;; lang-rust.el --- Rust configuration -*- lexical-binding: t; -*-
(require 'core-lsp)
(require 'core-shackle)
(require 'core-tree-sitter)

(use-package rustic
  :custom
  (rustic-format-on-save t)
  ;; No ligatures.
  (rust-prettify-symbols-alist nil)
  ;; We use eglot and prefer rust-analyzer over rls.
  (rustic-lsp-server 'rust-analyzer)
  (rustic-lsp-client 'eglot)
  ;; We use our own `+enable-lsp-maybe' function to enable eglot.
  (rustic-lsp-setup-p nil)
  :functions (+update-cargo-bin-a
              rustic-run-cargo-command)
  :defines (rustic-cargo-bin)
  :general
  (:prefix ef-local-leader :states 'normal :keymaps 'rustic-mode-map
   "c"  '(nil :wk "Cargo")
   "cC" '(rustic-cargo-check :wk "Check")
   "ca" '(+rust-cargo-audit :wk "Audit")
   "cb" '(rustic-cargo-bench :wk "Bench")
   "cc" '(rustic-cargo-build :wk "Build")
   "cd" '(rustic-cargo-build-doc :wk "Build docs")
   "cf" '(rustic-cargo-fmt :wk "Format")
   "co" '(rustic-cargo-outdated :wk "Outdated")
   "cr" '(rustic-cargo-run :wk "Run")
   "cy" '(rustic-cargo-clippy :wk "Clippy")

   "t"  '(nil :wk "Test")
   "ta" '(rustic-cargo-test :wk "At point")
   "tp" '(rustic-cargo-current-test :wk "At point"))
  :config
  (+enable-lsp rustic-mode)
  (+enable-tree-sitter rustic-mode)

  (poe-popup "^\\*rust" :ephemeral t :regexp t)
  (poe-popup "^\\*cargo" :ephemeral t :regexp t)

  (+add-hook rustic-mode-hook
    (setq-local mode-name "Rust"))

  (defun +rust-cargo-audit ()
    "Run \"cargo audit\" for the current project."
    (interactive)
    (rustic-run-cargo-command "cargo audit"))

  ;; Set `rustic-cargo-bin' globally in "real-time" so rustic sees get the
  ;; correct one from PATH set via `envrc'.
  ;;
  ;; We can't `setq-local' this in a major-mode hook as rustic configures it's
  ;; workspace wrapped in `with-temp-buffer' which has a different environment.
  (defun +update-cargo-bin-a (&rest _)
    ;; Don't update the path when we're in the temp-buffer, which has
    ;; `fundamental-mode' as it's major-mode.
    (unless (eq major-mode 'fundamental-mode)
      (setq rustic-cargo-bin (or (executable-find "cargo")
                                 "cargo"))))

  (advice-add 'rustic-cargo-bin :before #'+update-cargo-bin-a)

  ;; Don't ask to install LSP server if it's not installed.
  (advice-add 'rustic-install-lsp-client-p :override #'ignore))

(provide 'lang-rust)
