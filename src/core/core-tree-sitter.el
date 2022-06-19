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

(use-package evil-textobj-tree-sitter
  :after (evil tree-sitter)
  :functions (evil-textobj-tree-sitter-get-textobj
               evil-textobj-tree-sitter-goto-textobj)
  :config
  (defvar evil-normal-state-map)
  (defvar evil-inner-text-objects-map)
  (defvar evil-outer-text-objects-map)

  (define-key evil-outer-text-objects-map "B"
    (evil-textobj-tree-sitter-get-textobj "block.outer"))
  (define-key evil-inner-text-objects-map "B"
    (evil-textobj-tree-sitter-get-textobj "block.inner"))

  (define-key evil-outer-text-objects-map "F"
    (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "F"
    (evil-textobj-tree-sitter-get-textobj "function.inner"))

  (define-key evil-outer-text-objects-map "C"
    (evil-textobj-tree-sitter-get-textobj "class.outer"))
  (define-key evil-inner-text-objects-map "C"
    (evil-textobj-tree-sitter-get-textobj "class.inner"))

  (define-key evil-outer-text-objects-map "S"
    (evil-textobj-tree-sitter-get-textobj "statement.outer"))
  (define-key evil-inner-text-objects-map "S"
    (evil-textobj-tree-sitter-get-textobj "statement.inner"))

  (define-key evil-outer-text-objects-map "A"
    (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer")))

  (define-key evil-inner-text-objects-map "A"
    (evil-textobj-tree-sitter-get-textobj ("conditional.inner" "loop.inner")))

  ;; Goto start of next function
  (define-key evil-normal-state-map (kbd "]f")
    (lambda ()
      (interactive)
      (evil-textobj-tree-sitter-goto-textobj "function.outer")))

  ;; Goto start of previous function
  (define-key evil-normal-state-map (kbd "[f")
    (lambda ()
      (interactive)
      (evil-textobj-tree-sitter-goto-textobj "function.outer" t)))

  ;; Goto end of next function
  (define-key evil-normal-state-map (kbd "]F")
    (lambda ()
      (interactive)
      (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t)))

  ;; Goto end of previous function
  (define-key evil-normal-state-map (kbd "[F")
    (lambda ()
      (interactive)
      (evil-textobj-tree-sitter-goto-textobj "function.outer" t t))))

(provide 'core-tree-sitter)
