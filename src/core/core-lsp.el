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
  (lsp-prefer-flymake nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-gopls-experimental-staticcheck nil)
  :general
  (:states 'normal :keymap 'lsp-mode-map :prefix ef-prefix
	   "," '(lsp-find-definition "Find Definition"))
  :config
  (ef-add-popup "*lsp-performance*" :ephemeral 5 :size 0.15)
  (require 'lsp-mode)
  (require 'lsp-modeline)
  (require 'lsp-headerline))

(provide 'core-lsp)
