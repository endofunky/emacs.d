(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)

  (smartparens-global-strict-mode t)
  (show-smartparens-global-mode -1)

  (setq sp-highlight-pair-overlay nil)
  (setq sp-highlight-wrap-overlay nil)
  (setq sp-highlight-wrap-tag-overlay nil)

  (setq sp-autoescape-string-quote nil
        sp-autoskip-closing-pair 'always
        sp-show-pair-delay 0
        sp-show-pair-from-inside t)

  ;; Disable ' pairing where it makes sense.
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'ielm-mode "'" nil :actions nil)
  (sp-local-pair 'lisp-mode "'" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'lisp-interaction-mode "'" nil :actions nil)
  (sp-local-pair 'calc-mode "'" nil :actions nil)
  (sp-local-pair 'tex-mode "'" nil :actions nil)
  (sp-local-pair 'latex-mode "'" nil :actions nil)
  (sp-local-pair 'text-mode "'" nil :actions nil)
  (sp-local-pair 'log-edit-mode "'" nil :actions nil)
  (sp-local-pair 'org-mode "'" nil :actions nil)

  ;; Enable smartparens in minibuffer
  (setq sp-ignore-modes-list
        (delete 'minibuffer-inactive-mode sp-ignore-modes-list))
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

  (defun ts/sp-skip-asterisk (ms mb me)
    (save-excursion
      (goto-char mb)
      (save-match-data (looking-at "^\\* "))))

  (sp-with-modes '(markdown-mode gfm-mode)
    (sp-local-pair "`" "`"
                   :unless '(sp-point-after-word-p sp-point-at-bol-p)
                   :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "*" "*"
                   :unless '(sp-point-after-word-p sp-point-at-bol-p)
                   :skip-match 'ts/sp-skip-asterisk
                   :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "_" "_"))

  (sp-with-modes '(c-mode cc-mode)
    (sp-local-pair "#include <" ">"))

  (defun ts/sp-create-newline-and-enter-sexp (&rest _ignored)
    "Open a new brace or bracket expression, with relevant newlines and indent. "
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))

  (sp-local-pair '(c-mode scala-mode coffee-mode ruby-mode) "{" nil :post-handlers '((ts/sp-create-newline-and-enter-sexp "RET")))
  (sp-local-pair '(coffee-mode ruby-mode) "[" nil :post-handlers '((ts/sp-create-newline-and-enter-sexp "RET")))
  (sp-local-pair '(scala-mode) "(" nil :post-handlers '((ts/sp-create-newline-and-enter-sexp "RET"))))

(provide 'pkg-smartparens)
