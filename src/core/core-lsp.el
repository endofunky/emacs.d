(require 'core-evil)
(require 'core-shackle)

(use-package lsp-mode
  :commands lsp
  :ensure t
  :custom
  (lsp-auto-guess-root t)
  (lsp-eldoc-enable-hover t)
  (lsp-enable-snippet nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-lens-enable nil)
  (lsp-prefer-flymake nil)
  (lsp-signature-doc-lines 1)
  (lsp-ui-remap-xref-keybindings nil)
  (lsp-use-lsp-ui nil)
  :config
  (require 'lsp-mode)
  (require 'lsp-modeline)
  (require 'lsp-headerline)
  (ef-add-popup "*lsp-performance*" :ephemeral t :size 0.15)
  (ef-add-popup "*lsp-help*" :ephemeral t :size 0.2))

(ef-deflang lsp
  :compile-backend-connect lsp
  :compile-backend-reconnect lsp-workspace-restart
  :compile-backend-quit lsp-disconnect
  :compile-nav-jump lsp-find-definition
  :compile-nav-pop-back pop-tag-mark

  :refactor-imports lsp-organize-imports
  :refactor-rename lsp-rename

  :xref-apropos xref-find-apropos
  :xref-definitions lsp-find-declaration
  :xref-references lsp-find-references)

(provide 'core-lsp)
