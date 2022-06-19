(require 'core-direnv)

(use-package eglot
  :custom
  (eglot-sync-connect 1)
  (eglot-connect-timeout 10)
  (eglot-autoshutdown t)
  (eglot-auto-display-help-buffer nil)
  (eglot-ignored-server-capabilities '(:documentHighlightProvider
                                       :documentSymbolProvider))
  :commands (eglot
             eglot-ensure
             eglot-code-action-organize-imports
             ef-enable-lsp-maybe)
  :config
  (ef-add-popup "^\\*eglot-help")

  (defun ef-lsp-organize-imports ()
    (interactive)
    (when (bound-and-true-p eglot--managed-mode)
      (call-interactively #'eglot-code-action-organize-imports)))

  (defun ef-enable-lsp-maybe ()
    (interactive)
    (when buffer-file-name
      (when (and (fboundp 'envrc--update))
        (envrc--update))
      (eglot-ensure))))

(use-package consult-eglot
  :after (eglot consult)
  :general
  (:keymaps 'eglot-mode-map
   [remap xref-find-apropos] 'consult-eglot-symbols)
  :commands (consult-eglot-symbols))

;; (ef-deflang lsp
;;   :compile-backend-connect lsp
;;   :compile-backend-reconnect lsp-workspace-restart
;;   :compile-backend-quit lsp-disconnect
;;   :compile-nav-jump lsp-find-definition
;;   :compile-nav-pop-back pop-tag-mark

;;   :doc-point lsp-describe-thing-at-point

;;   :refactor-imports lsp-organize-imports
;;   :refactor-rename lsp-rename

;;   :xref-apropos consult-lsp-symbols
;;   :xref-definitions lsp-find-declaration
;;   :xref-references lsp-find-references)

(provide 'core-lsp)
