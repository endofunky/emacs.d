(require 'core-direnv)
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

(ef-deflang eglot
  :after eglot

  :compile-backend-connect eglot
  :compile-backend-reconnect eglot-reconnect
  :compile-backend-quit eglot-shutdown
  :compile-nav-jump xref-find-definitions
  :compile-nav-pop-back pop-tag-mark

  :refactor-imports ef-lsp-organize-imports
  :refactor-rename eglot-rename

  :xref-apropos consult-eglot-symbols
  :xref-definitions xref-find-definitions)

(provide 'core-lsp)
