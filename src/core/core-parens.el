(require 'core-evil)

(use-package paren
  :custom
  (show-paren-priority -50) ; Highlight parens in region
  (show-paren-delay 0)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  :config
  (show-paren-mode t))

(use-package smartparens
  :demand t
  :custom
  (sp-autoskip-closing-pair 'always)
  (sp-highlight-pair-overlay nil)
  (sp-highlight-wrap-overlay nil)
  (sp-highlight-wrap-tag-overlay nil)
  (sp-show-pair-delay 0)
  (sp-show-pair-from-inside t)
  :commands (show-smartparens-global-mode
             smartparens-global-mode)
  :functions (sp-with-modes)
  :config
  (declare-function sp-local-pair "smartparens")
  (show-smartparens-global-mode -1)
  (smartparens-global-mode t)

  (defun ef-sp-skip-asterisk (ms mb me)
    "Skip asterisk if at begging of line"
    (save-excursion
      (goto-char mb)
      (save-match-data (looking-at "^\\* "))))

  (defun ef-sp-create-newline-and-enter-sexp (&rest _ignored)
    "Open a new brace or bracket expression, with relevant newlines and indent"
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))

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

  (sp-with-modes '(c-mode c++-mode cc-mode)
    (sp-local-pair "#include <" ">")

    (sp-local-pair "[" nil :post-handlers '(:add
                                            (ef-sp-create-newline-and-enter-sexp "RET")))

    (sp-local-pair "{" nil :post-handlers '(:add ef-c-maybe-add-semicolon-brace
                                            (ef-sp-create-newline-and-enter-sexp "RET")))

    (sp-local-pair "(" nil :post-handlers '(:add
                                            ef-c-maybe-add-semicolon-paren
                                            (ef-sp-create-newline-and-enter-sexp "RET"))))

  (sp-local-pair 'c++-mode "[" nil :post-handlers '(:add
                                                    maybe-complete-lambda
                                                    (ef-sp-create-newline-and-enter-sexp "RET")))


  (sp-with-modes '(go-mode)
    (sp-local-pair "{" nil :post-handlers '((ef-sp-create-newline-and-enter-sexp "RET"))))

  (sp-local-pair 'markdown-mode "'" nil :actions nil)

  (sp-with-modes '(markdown-mode)
    (sp-local-pair "`" "`"
                   :unless '(sp-point-after-word-p sp-point-at-bol-p)
                   :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "*" "*"
                   :unless '(sp-point-after-word-p sp-point-at-bol-p)
                   :skip-match 'ef-sp-skip-asterisk
                   :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "_" "_"))

  (ef-add-hook ruby-mode-hook :interactive t
    (require 'smartparens-ruby)

    (sp-with-modes '(ruby-mode)
      (sp-local-pair "[" nil :post-handlers '((ef-sp-create-newline-and-enter-sexp "RET")))
      (sp-local-pair "{" nil :post-handlers '((ef-sp-create-newline-and-enter-sexp "RET")))
      (sp-local-pair "(" nil :post-handlers '((ef-sp-create-newline-and-enter-sexp "RET"))))))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package lispy
  :hook ((common-lisp-mode . lispy-mode)
         (emacs-lisp-mode . lispy-mode)
         (scheme-mode . lispy-mode)
         (ielm-mode . lispy-mode)
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
  :after lispy
  :hook (lispy-mode . lispyville-mode)
  :general
  (:states '(normal visual) :keymaps 'lispyville-mode-map
   "\\" 'lispyville-comment-or-uncomment-line
   "#" 'lispyville-comment-or-uncomment-line)
  :config
  (declare-function lispyville-set-key-theme "lispyville")

  (lispyville-set-key-theme
   '((operators normal)
     c-w
     commentary
     additional-wrap
     slurp/barf-cp
     text-objects)))

(provide 'core-parens)
