(use-package tree-sitter
  :if module-file-suffix
  :ensure t
  :demand t
  :commands (global-tree-sitter-mode)
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :if module-file-suffix
  :ensure t
  :commands (tree-sitter-hl-mode))

(provide 'core-tree-sitter)
