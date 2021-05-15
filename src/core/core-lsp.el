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
  (lsp-prefer-flymake :none)
  :general
  (:states 'normal :keymap 'lsp-mode-map :prefix ef-prefix
	   "," 'lsp-find-definition))

(provide 'core-lsp)
