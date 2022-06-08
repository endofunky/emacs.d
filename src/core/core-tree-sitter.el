(require 'use-package)

(use-package tree-sitter
  :demand t
  :straight (tree-sitter :type git
                         :host github
                         :repo "ubolonton/emacs-tree-sitter"
                         :files ("lisp/*.el"))
  :hook
  (tree-sitter-after-on . tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :after tree-sitter
  :straight (tree-sitter-langs :type git
                               :host github
                               :repo "ubolonton/emacs-tree-sitter"
                               :files ("langs/*.el" "langs/queries"))
  :config
  (global-tree-sitter-mode))

(provide 'core-tree-sitter)
