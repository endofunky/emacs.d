(require 'core-direnv)
(require 'core-shackle)

(use-package eglot
  :custom
  (eglot-sync-connect 1)
  (eglot-connect-timeout 10)
  (eglot-autoshutdown t)
  (eglot-auto-display-help-buffer nil)
  (eglot-send-changes-idle-time 0.25)
  (eglot-ignored-server-capabilities '(:documentHighlightProvider
                                       :documentSymbolProvider))
  :commands (eglot
             eglot-ensure
             eglot-code-action-organize-imports
             ef-enable-lsp-maybe)
  :config
  (declare-function eglot--guess-contact "ext:eglot")

  (ef-add-popup "^\\*eglot-help")

  (defun ef-lsp-organize-imports ()
    (interactive)
    (when (bound-and-true-p eglot--managed-mode)
      (call-interactively #'eglot-code-action-organize-imports)))

  (defun ef-enable-lsp-maybe ()
    "Enable `eglot' if a LSP server matching the major-mode can be found."
    (interactive)
    (when buffer-file-name
      (when (and (fboundp 'envrc-global-mode-check-buffers))
        (envrc-global-mode-check-buffers))
      (if-let (srv (car (plist-get (eglot--guess-contact) 'eglot-lsp-server)))
          (if (or (file-exists-p srv)
                  (locate-file srv exec-path exec-suffixes 1))
              (eglot-ensure))))))

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
