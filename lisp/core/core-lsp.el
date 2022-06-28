;;; core-lsp.el --- Language server protocol integration -*- lexical-binding: t; -*-
(require 'core-direnv)
(require 'core-evil)
(require 'core-shackle)

(use-package eglot
  :custom
  (eglot-sync-connect 1)
  (eglot-connect-timeout 10)
  (eglot-autoshutdown t)
  (eglot-auto-display-help-buffer nil)
  (eglot-send-changes-idle-time 0.1)
  (eglot-ignored-server-capabilities '(:documentHighlightProvider
                                       :documentSymbolProvider))
  ;; Consider files outside project jumped to via xref part of the project.
  (eglot-extend-to-xref t)
  :general
  (:states 'normal :keymaps 'eglot-mode-map
   "C-t" 'xref-go-back
   "gd" 'xref-find-definitions
   "gr" 'xref-find-references
   "K" 'ef-eglot-lookup-documentation)
  (:prefix ef-leader :states 'normal :keymaps 'eglot-mode-map
   "L"  '(nil :wk "LSP")
   "Lc" '(eglot :wk "Reconnect")
   "Lr" '(eglot-reconnect :wk "Reconnect")
   "Lq" '(eglot-shutdown :wk "Quit")
   "R"  '(nil :wk "Refactor")
   "Ri" '(ef-lsp-organize-imports :wk "Organize imports")
   "Rr" '(eglot-rename :wk "Rename"))
  :commands (eglot
             eglot-ensure
             eglot-code-action-organize-imports
             ef-enable-lsp-maybe)
  :defines (eglot-server-programs)
  :functions (eglot-lsp-server
              eglot--hover-info
              eglot--TextDocumentPositionParams
              eglot--current-server-or-lose
              eglot-alternatives)
  :config
  (declare-function eglot--guess-contact "ext:eglot")

  (ef-add-popup "^\\*eglot-help" :regexp t :size 0.4)

  (defun ef-lsp-organize-imports ()
    (interactive)
    (when (bound-and-true-p eglot--managed-mode)
      (call-interactively #'eglot-code-action-organize-imports)))

  (defun ef-enable-lsp-maybe ()
    "Enable `eglot' if a LSP server matching the major-mode can be found."
    (interactive)
    (when (and buffer-file-name)
      (when (and (fboundp 'envrc-global-mode-check-buffers)
                 (not (file-remote-p default-directory)))
        (envrc-mode t))
      (eglot-ensure)))

  ;; Pass the correct initialization parameters to solargraph to make language
  ;; server diagnostics work.
  (defclass eglot-solargraph (eglot-lsp-server) ()
    :documentation "A custom class for solargraph's Ruby langserver.")

  (cl-defmethod eglot-initialization-options ((server eglot-solargraph))
    "Passes through required solargraph initialization options."
    (list :diagnostics t))

  (assoc-delete-all 'ruby-mode eglot-server-programs)

  (add-to-list 'eglot-server-programs
               '(ruby-mode . (eglot-solargraph "solargraph"
                                               "socket"
                                               "--port" :autoport)))

  ;; Prefer texlab over digestif
  (assoc-delete-all '(tex-mode context-mode texinfo-mode bibtex-mode)
                    eglot-server-programs)

  (add-to-list 'eglot-server-programs
               `((tex-mode context-mode texinfo-mode bibtex-mode)
                 .
                 ,(eglot-alternatives '(("texlab") ("digestif")))))

  ;; Add command-line options to clangd to increase number of background jobs,
  ;; enable background indexing, clang-tidy and disable bundling of completions
  ;; and header insertion..
  (assoc-delete-all '(c++-mode c-mode) eglot-server-programs)

  (add-to-list 'eglot-server-programs
               `((c++-mode c-mode)
                 .
                 ,(eglot-alternatives '(("ccls")
                                        ("clangd"
                                         "-j=4"
                                         "--background-index"
                                         "--clang-tidy"
                                         "--completion-style=detailed"
                                         "--header-insertion=never")))))


  ;; The following is taken from doom-emacs' eglot.el:
  ;; https://github.com/doomemacs/doomemacs/blob/master/modules/tools/lsp/autoload/eglot.el
  ;;
  ;; HACK Eglot removed `eglot-help-at-point' in joaotavora/eglot@a044dec for a
  ;;      more problematic approach of deferred to eldoc.
  (declare-function jsonrpc-request "ext:jsonrpc")

  (defvar ef-eglot--help-buffer nil)

  (defun ef-eglot-lookup-documentation (_identifier)
    "Request documentation for the thing at point."
    (interactive "P")
    (eglot--dbind ((Hover) contents range)
                  (jsonrpc-request (eglot--current-server-or-lose) :textDocument/hover
                                   (eglot--TextDocumentPositionParams))
                  (let ((blurb (and (not (seq-empty-p contents))
                                    (eglot--hover-info contents range)))
                        (hint (thing-at-point 'symbol)))
                    (if blurb
                        (with-current-buffer
                            (or (and (buffer-live-p ef-eglot--help-buffer)
                                     ef-eglot--help-buffer)
                                (setq ef-eglot--help-buffer
                                      (generate-new-buffer "*eglot-help*")))
                          (with-help-window (current-buffer)
                            (rename-buffer (format "*eglot-help for %s*" hint))
                            (with-current-buffer standard-output (insert blurb))
                            (setq-local nobreak-char-display nil)))
                      (display-local-help))))
    'deferred))

(use-package consult-eglot
  :after (eglot consult)
  :general
  (:keymaps 'eglot-mode-map
   [remap xref-find-apropos] 'consult-eglot-symbols)
  :commands (consult-eglot-symbols))

(provide 'core-lsp)
