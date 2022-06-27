(require 'core-lib)

(use-package graphviz-dot-mode
  :mode (("\\.dot\\'" . graphviz-dot-mode)
         ("\\.gv\\'" . graphviz-dot-mode))
  :commands (graphviz-dot-mode)
  :custom
  (graphviz-dot-indent-width 2))

(use-package org-src
  :defer t
  :straight nil
  :config
  (add-to-list 'org-src-lang-modes  '("dot" . graphviz-dot)))

(provide 'lang-dot)
