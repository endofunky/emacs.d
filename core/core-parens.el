(require 'core-evil)

(use-package paren
  :custom
  (show-paren-delay 0)
  :config
  (show-paren-mode t))

(use-package smartparens
  :ensure t
  :custom
  (sp-autoskip-closing-pair 'always)
  (sp-highlight-pair-overlay nil)
  (sp-highlight-wrap-overlay nil)
  (sp-highlight-wrap-tag-overlay nil)
  (sp-show-pair-delay 0)
  (sp-show-pair-from-inside t)
  :config
  (declare-function sp-local-pair "smartparens")
  (show-smartparens-global-mode -1)
  (smartparens-global-mode t)

  ;; Disable ' pairing where it makes sense.
  (sp-local-pair 'calc-mode "'" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'ielm-mode "'" nil :actions nil)
  (sp-local-pair 'latex-mode "'" nil :actions nil)
  (sp-local-pair 'lisp-interaction-mode "'" nil :actions nil)
  (sp-local-pair 'lisp-mode "'" nil :actions nil)
  (sp-local-pair 'log-edit-mode "'" nil :actions nil)
  (sp-local-pair 'org-mode "'" nil :actions nil)
  (sp-local-pair 'tex-mode "'" nil :actions nil)
  (sp-local-pair 'text-mode "'" nil :actions nil)

  (defun ef-sp-create-newline-and-enter-sexp (&rest _ignored)
    "Open a new brace or bracket expression, with relevant newlines and indent"
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode)))

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package lispy
  :ensure t
  :hook ((common-lisp-mode . lispy-mode)
         (emacs-lisp-mode . lispy-mode)
         (scheme-mode . lispy-mode)
         (racket-mode . lispy-mode)
         (hy-mode . lispy-mode)
         (lfe-mode . lispy-mode)
         (clojure-mode . lispy-mode))
  :config
  (declare-function lispy-set-key-theme "lispy")

  (lispy-set-key-theme '(paredit c-digits))

  (ef-add-hook lispy-mode-hook
    (if (fboundp 'turn-off-smartparens-mode)
        (turn-off-smartparens-mode))))

(use-package lispyville
  :ensure t
  :after lispy
  :hook (lispy-mode . lispyville-mode)
  :general
  (:states '(normal visual) :keymaps 'lispyville-mode-map
	   "\\" 'lispyville-comment-or-uncomment-line
	   "#" 'lispyville-comment-or-uncomment-line)
  :config
  (lispyville-set-key-theme
   '((operators normal)
     c-w
     commentary
     additional-wrap
     slurp/barf-cp)))

(provide 'core-parens)
