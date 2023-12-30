;;; lang-rust.el --- Rust configuration -*- lexical-binding: t; -*-
(require 'core-direnv)
(require 'core-eglot)
(require 'core-popup)

(use-package rust-ts-mode
  :straight nil
  :mode ("\\.rs\\'" . rust-ts-mode)
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
  (poe-popup "*rustfmt*" :ephemeral t)
  (+add-hook rust-ts-mode-hook
    (setq-local fill-column 100)
    (add-hook 'before-save-hook 'rust-format-buffer nil t)))

(use-package cargo-mode
  :hook
  (rust-ts-mode . cargo-minor-mode)
  :functions (+cargo-minor-mode-hook
              cargo-mode--project-directory
              cargo-mode--start)
  :general
  (:prefix ef-local-leader :states 'normal :keymaps 'cargo-minor-mode-map
   "c"  '(nil :wk "Cargo")
   "ce" '(cargo-mode-execute-task :wk "Execute task")
   "cc" '(cargo-mode-build :wk "Build")
   "cr" '(+cargo-mode-run :wk "Run")

   "l"  '(nil :wk "Lint")
   "ly" '(+cargo-mode-clippy :wk "Clippy")

   "t"  '(nil :wk "Test")
   "ta" '(cargo-mode-test :wk "All")
   "tt" '(cargo-mode-test-current-buffer :wk "Buffer")
   "tp" '(cargo-mode-test-current-test :wk "Point"))
  :config
  (poe-popup 'cargo-mode :size .5 :select nil)

  (defun +cargo-mode-run ()
    "Execute cargo run."
    (interactive)
    (let* ((project-root (cargo-mode--project-directory)))
      (cargo-mode--start "execute" "run" project-root nil)))

  (defun +cargo-mode-clippy ()
    "Execute cargo clippy."
    (interactive)
    (let* ((project-root (cargo-mode--project-directory)))
      (cargo-mode--start "execute" "clippy" project-root nil)))

  (+add-hook cargo-minor-mode-hook :fn +cargo-minor-mode-hook
    (setq-local cargo-path-to-bin (or (executable-find "cargo")
                                      "~/.cargo/bin/cargo"))))

(provide 'lang-rust)
