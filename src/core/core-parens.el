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
  :functions (sp-with-modes
                 sp-local-pair)
  :config
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
                                                    ef-c-maybe-complete-lambda
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

  (ef-add-hook rustic-mode-hook :interactive t
    (sp-with-modes '(rustic-mode)
      (sp-local-pair "[" nil :post-handlers '((ef-sp-create-newline-and-enter-sexp "RET")))
      (sp-local-pair "{" nil :post-handlers '((ef-sp-create-newline-and-enter-sexp "RET")))
      (sp-local-pair "(" nil :post-handlers '((ef-sp-create-newline-and-enter-sexp "RET")))))

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

;; https://github.com/Hi-Angel/dotfiles/blob/bbd08c6883daed98b9feaad7f86304d332f51e3d/.emacs#L583-L642
(defun ef-c-is-in-comment ()
  "tests if point is in comment"
  (nth 4 (syntax-ppss)))

(defun ef-c-current-line-string ()
  "returns current line as a string"
  (buffer-substring-no-properties (line-beginning-position)
                                  (line-end-position)))

(defvar ef-c-iter-kw
  '("if" "else" "switch" "for" "while" "do" "define" "rep" "rrep" "trav"))

(defun ef-c-maybe-add-semicolon-paren (_id action _context)
  "A helper function that inserts semicolon after closing
parentheses when appropriate. Mainly useful in C, C++, and other
languages with similar syntax"
  (when (eq action 'insert)
    (save-excursion
      ;; here, caret supposed to be in between parens, i.e. (|)
      (forward-char) ;; skip closing brace
      (when (and (looking-at "\\s-*$")
                 (not (string-match-p
                       (regexp-opt ef-c-iter-kw 'words)
                       (ef-c-current-line-string)))
                 (not (ef-c-is-in-comment)))
        (insert ";")))))

(defun ef-c-maybe-add-semicolon-brace (_id action _context)
  "A helper function that inserts semicolon after closing
parentheses when appropriate. Mainly useful in C, C++, and other
languages with similar syntax"
  (when (eq action 'insert)
    (save-excursion
      ;; here, caret supposed to be in between parens, i.e. {|}
      (forward-char) ;; skip closing brace
      (when (and (looking-at "\\s-*$")
                 (string-match-p "\\breturn\\b" (ef-c-current-line-string))
                 (not (ef-c-is-in-comment)))
        (insert ";")))))

(defun ef-c-maybe-complete-lambda (_id action _context)
  "Completes C++ lambda, given a pair of square brackets"
  (when (eq action 'insert)
    (let ((curr-line (ef-c-current-line-string))
          ;; try detecting "auto foo = []"
          (lambda-assign-regex "=\\s-*\\[\\]$")
          ;; try detecting "func([])" and "func(arg1, [])"
          (lambda-inline-regex "[(,]\\s-*\\[\\]"))
      (when (or (string-match-p lambda-assign-regex curr-line)
                (string-match-p lambda-inline-regex curr-line))
        (save-excursion
          ;; here, caret supposed to be in between brackets, i.e. [|]
          (forward-char) ;; skip closing brace
          (insert "() {}")
          (when (eolp)
            (insert ";")))))))

(provide 'core-parens)
