;;; core-tree-sitter.el --- Tree-sitter integration -*- lexical-binding: t; -*-
(require 'core-lib)

(use-package tree-sitter
  :defer t
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
                               :files ("langs/*.el" "langs/queries")))

(defmacro ef-tree-sitter (&rest modes)
  "Enable `tree-sitter-mode' for MODES."
  (let* ((m (ef-as-list modes))
         (hooks (mapcar (lambda (mode)
                          `(add-hook ',(ef-mode-hook mode) 'tree-sitter-mode))
                        m)))
    (macroexp-progn hooks)))

(provide 'core-tree-sitter)
