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
              cargo-mode--start
              rust-in-str-or-cmnt
              rust-beginning-of-defun)
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
                                      "~/.cargo/bin/cargo")))

  ;; The following functions are imported from rustic and are required to make
  ;; `cargo-mode-test-current-test' work.
  (defun rust-in-str-or-cmnt () (nth 8 (syntax-ppss)))

  (defvar rust-func-item-beg-re
    (concat "\\s-*\\(?:priv\\|pub\\)?\\s-*\\(?:async\\)?\\s-*"
            (regexp-opt '("fn")))
    "Start of a rust function.")

  ;; this function only differs from the rust-mode version in applying
  ;; `rustic-func-item-beg-re'
  (defun rust-beginning-of-defun (&optional arg)
    (interactive "p")
    (let* ((arg (or arg 1))
           (magnitude (abs arg))
           (sign (if (< arg 0) -1 1)))
      ;; If moving forward, don't find the defun we might currently be
      ;; on.
      (when (< sign 0)
        (end-of-line))
      (catch 'done
        (dotimes (_ magnitude)
          ;; Search until we find a match that is not in a string or comment.
          (while (if (re-search-backward (concat "^\\(" rust-func-item-beg-re "\\)")
                                         nil 'move sign)
                     (rust-in-str-or-cmnt)
                   ;; Did not find it.
                   (throw 'done nil)))))
      t)))

(provide 'lang-rust)
