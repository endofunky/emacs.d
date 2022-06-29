;;; lang-dot.el --- GraphViz dot configuration -*- lexical-binding: t; -*-
(require 'core-lib)

(use-package graphviz-dot-mode
  :mode (("\\.dot\\'" . graphviz-dot-mode)
         ("\\.gv\\'" . graphviz-dot-mode))
  :commands (graphviz-dot-mode)
  :general
  (:prefix ef-local-leader :states 'visual :keymaps 'graphviz-dot-mode-map
   "c" '(nil :wk "Compile")
   "cc" '(compile :wk "Compile")
   "cv" '(graphviz-dot-view :wk "View")
   "cp" '(graphviz-dot-preview :wk "Preview"))
  :custom
  (graphviz-dot-indent-width 2))

(use-package org-src
  :defer t
  :straight nil
  :config
  (add-to-list 'org-src-lang-modes  '("dot" . graphviz-dot)))

(provide 'lang-dot)
