(require 'core-lib)
(require 'core-direnv)
(require 'core-evil)
(require 'core-shackle)
(require 'core-project)

(use-package lsp-mode
  :commands lsp
  :custom
  (lsp-auto-execute-action nil)
  (lsp-auto-guess-root t)
  (lsp-completion-show-kind nil)
  (lsp-enable-folding nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-snippet nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-text-document-color nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-lens-enable nil)
  (lsp-enable-server-download nil)
  (lsp-signature-auto-activate nil)
  (lsp-signature-doc-lines 1)
  (lsp-signature-render-documentation nil)
  (lsp-ui-doc-enable nil)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-show-hover nil)
  :hook
  ;; Sometime evil-mode gets into a weird state when lsp gets enabled in a hook
  ;; and keybinds don't appear to work correctly, so force it into normal state
  ;; when lsp-mode gets enabled.
  (lsp-mode . evil-normal-state)
  :functions (lsp-organize-imports
              lsp--require-packages
              lsp--filter-clients
              ef-lsp-envrc-ad)
  :config
  (require 'lsp-modeline)
  (require 'lsp-headerline)
  (require 'lsp-ui)
  (require 'lsp-diagnostics)

  (defun ef-lsp-organize-imports ()
    "Call `lsp-organize-imports' with `lsp-auto-execute-action' enabled to skip
the confirmation prompt when called via `before-save-hook', for example."
    (when (bound-and-true-p lsp-mode)
      (let ((lsp-auto-execute-action t))
        (call-interactively #'lsp-organize-imports))))

  (if (boundp 'read-process-output-max)
      (setq read-process-output-max (* 1024 1024)))

  (when (featurep 'envrc)
    (defun ef-lsp-envrc-ad (orig-fun &rest args)
      "Update envrc environment and enable `lsp' when the current buffer's
file is part of a project and a supported lsp binary is present."
      (envrc--update)
      (when (project-current)
        ;; Ensure lsp packages have been required so all the major-mode to LSP
        ;; server mappings exist.
        (lsp--require-packages)
        ;; This is also being done inside `lsp', but to avoid any missing
        ;; server warnings in the mode-line we check for servers ourselves
        ;; here, too.
        (if (lsp--filter-clients
             (-andfn #'lsp--supports-buffer?
                     #'lsp--server-binary-present?))
            (apply orig-fun args))))

    (advice-add 'lsp :around #'ef-lsp-envrc-ad))

  (ef-add-popup "*lsp-performance*" :ephemeral t :size 0.15)
  (ef-add-popup "*lsp session*" :ephemeral t)
  (ef-add-popup "*lsp-help*" :ephemeral t :size 0.2))

(use-package lsp-ui
  :defer t)

(ef-deflang lsp
  :compile-backend-connect lsp
  :compile-backend-reconnect lsp-workspace-restart
  :compile-backend-quit lsp-disconnect
  :compile-nav-jump lsp-find-definition
  :compile-nav-pop-back pop-tag-mark

  :doc-point lsp-describe-thing-at-point

  :refactor-imports lsp-organize-imports
  :refactor-rename lsp-rename

  :xref-apropos consult-lsp-symbols
  :xref-definitions lsp-find-declaration
  :xref-references lsp-find-references)

(provide 'core-lsp)
