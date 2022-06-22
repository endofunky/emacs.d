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
  ;; Disable smartparen's navigation
  (sp-navigate-skip-match nil)
  (sp-navigate-consider-sgml-tags nil)
  :commands (show-smartparens-global-mode
             smartparens-global-mode)
  :functions (sp-local-pair
              sp-pair
              ef-current-line-string
              ef-is-in-comment)
  :defines (sp-c-modes)
  :config
  (require 'smartparens-config)

  (show-smartparens-global-mode -1)
  (smartparens-global-mode t)

  ;; Don't autopair quotes when next to a word/before another quote in order to
  ;; not unbalance them.
  (let ((unless-list '(sp-point-before-word-p
                       sp-point-after-word-p
                       sp-point-before-same-p)))
    (sp-pair "'"  nil :unless unless-list)
    (sp-pair "\"" nil :unless unless-list))

  ;; Don't do square-bracket space-expansion where it doesn't make sense to
  (sp-local-pair '(emacs-lisp-mode org-mode markdown-mode gfm-mode)
                 "[" nil :post-handlers '(:rem ("| " "SPC")))

  ;; Expand {|} => { | }
  ;; Expand {|} => {
  ;;   |
  ;; }
  (dolist (brace '("(" "{" "["))
    (sp-pair brace nil
             :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))
             ;; Again, don't insert pair when next to a word/matching pair.
             :unless '(sp-point-before-word-p
                       sp-point-before-same-p)))

  ;; Pair /* .. */ documentation blocks correctly.
  (sp-local-pair
   '(js2-mode typescript-mode rjsx-mode rust-mode c-mode c++-mode objc-mode
              csharp-mode java-mode php-mode css-mode scss-mode less-css-mode
              stylus-mode scala-mode)
   "/*" "*/"
   :actions '(insert)
   :post-handlers '(("| " "SPC")
                    ("|\n[i]*/[d-2]" "RET")
                    (ef-default-expand-asterix-doc-comment-block "*")))

  ;; Auto-pair auto-documentation comment blocks in cc-mode derivatives.
  (sp-local-pair '(c-mode c++-mode objc-mode java-mode)
                 "/*!" "*/"
                 :post-handlers '(("* ||\n[i]" "RET") ("[d-1]< | " "SPC")))

  ;; Smartparens config for `ruby-mode'
  (with-eval-after-load 'smartparens-ruby
    ;; Disable the ruby pre-handler for braces.
    (sp-local-pair 'ruby-mode "{" "}"
                   :pre-handlers '(:rem sp-ruby-pre-handler)
                   :post-handlers '(:rem sp-ruby-post-handler))

    ;; Disable the pre-handler for pipes
    (sp-local-pair 'ruby-mode "|" "|"
                   :pre-handlers '(:rem sp-ruby-pre-pipe-handler)))

  ;; Smartparens config for `rust-mode'.
  (with-eval-after-load 'smartparens-rust
    ;; Insert semicolon after closing parentheses where appropriate in rust-mode.
    (defun ef-maybe-add-semicolon-paren-rust (_id action _context)
      "A helper function that inserts semicolon after closing
parentheses when appropriate, for Rust lang"
      ;; here, caret supposed to be in between parens, i.e. (|)
      (when (and (eq action 'insert)
                 (looking-at ")\\s-*$")
                 (not (ef-is-in-comment))
                 (not (string-match-p
                       (regexp-opt '("fn" "if" "for" "while") 'words)
                       (ef-current-line-string))))
        (save-excursion
          (forward-char) ;; skip closing brace
          (insert ";"))))

    (sp-local-pair 'rust-mode '"{" nil
                   :post-handlers '(:add ef-maybe-add-semicolon-paren-rust)))

  ;; Smartparens config for `markdown-mode'.
  (with-eval-after-load 'smartparens-markdown
    (defun ef-sp-skip-asterisk (ms mb me)
      "Skip asterisk if at begging of line"
      (save-excursion
        (goto-char mb)
        (save-match-data (looking-at "^\\* "))))

    (sp-local-pair '(markdown-mode gfm-mode) "`" "`"
                   :unless '(:add sp-point-before-word-p
                             sp-point-before-same-p))

    (sp-local-pair '(markdown-mode gfm-mode) "```" "```"
                   :post-handlers '(:add ("||\n" "RET")))

    (sp-local-pair '(markdown-mode gfm-mode) "*" "*"
                   :unless '(sp-point-after-word-p
                             sp-point-at-bol-p)
                   :skip-match 'ef-sp-skip-asterisk
                   :post-handlers '(("[d1]" "SPC")))

    (sp-local-pair '(markdown-mode gfm-mode) "_" "_"))

  ;; `objc-mode' appears to be missing from the defaults in `sp-c-mode', so we
  ;; add it here.
  (add-to-list 'sp-c-modes 'objc-mode)

  ;; Smartparens config for `sp-c-modes' (`c-mode', `c++-mode').
  (with-eval-after-load 'smartparens-c
    ;; Pair angle brackets after C/C++ #include statements.
    (sp-local-pair '(c-mode c++-mode objc-mode) "#include <" ">")

    ;; Expand C-style doc comment blocks. Must be done manually because some of
    ;; these languages use specialized (and deferred) parsers, whose state we
    ;; can't access while smartparens is doing its thing.
    (defun ef-default-expand-asterix-doc-comment-block (&rest _ignored)
      (let ((indent (current-indentation)))
        (newline-and-indent)
        (save-excursion
          (newline)
          (insert (make-string indent 32) " */")
          (delete-char 2))))

    ;; Insert semicolon after closing parentheses where appropriate in C/C++
    ;; mode.
    (defun ef-is-in-comment ()
      "tests if point is in comment"
      (nth 4 (syntax-ppss)))

    (defun ef-current-line-string ()
      "returns current line as a string"
      (buffer-substring-no-properties (line-beginning-position)
                                      (line-end-position)))

    (defvar ef-iter-kw
      '("if" "else" "switch" "for" "while" "do" "define" "rep" "rrep" "trav"))

    (defun ef-maybe-add-semicolon-paren (_id action _context)
      "A helper function that inserts semicolon after closing
  parentheses when appropriate. Mainly useful in C, C++, and other
  languages with similar syntax"
      (when (eq action 'insert)
        (save-excursion
          ;; here, caret supposed to be in between parens, i.e. (|)
          (forward-char) ;; skip closing brace
          (when (and (looking-at "\\s-*$")
                     (not (string-match-p
                           (regexp-opt ef-iter-kw 'words)
                           (ef-current-line-string)))
                     (not (ef-is-in-comment)))
            (insert ";")))))

    (defun ef-maybe-add-semicolon-brace (_id action _context)
      "A helper function that inserts semicolon after closing
  parentheses when appropriate. Mainly useful in C, C++, and other
  languages with similar syntax"
      (when (eq action 'insert)
        (save-excursion
          ;; here, caret supposed to be in between parens, i.e. {|}
          (forward-char) ;; skip closing brace
          (when (and (looking-at "\\s-*$")
                     (string-match-p "\\breturn\\b" (ef-current-line-string))
                     (not (ef-is-in-comment)))
            (insert ";")))))

    (sp-local-pair '(c-mode c++-mode objc-mode) '"{" nil
                   :post-handlers '(:add ef-maybe-add-semicolon-brace))

    (sp-local-pair '(c-mode c++-mode objc-mode) "(" nil
                   :post-handlers '(:add
                                    ef-maybe-add-semicolon-paren))

    ;; Auto-complete lambda blocks in C++
    ;;
    ;; Expand auto foo = [] => auto foo = [|]() {};
    ;;
    ;; Expand foo([]) => foo([]() {});
    ;;
    (defun ef-maybe-complete-lambda (_id action _context)
      "Completes C++ lambda, given a pair of square brackets"
      (when (eq action 'insert)
        (let ((curr-line (ef-current-line-string))
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

    (sp-local-pair 'c++-mode "[" nil
                   :post-handlers '(:add
                                    ef-maybe-complete-lambda))))

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
