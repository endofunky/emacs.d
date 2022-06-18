(require 'core-lsp)
(require 'core-shackle)
(require 'core-project)

;; https://github.com/Hi-Angel/dotfiles/blob/bbd08c6883daed98b9feaad7f86304d332f51e3d/.emacs#L583-L642
(defun ef-c-is-in-comment ()
  "tests if point is in comment"
  (nth 4 (syntax-ppss)))

(defun ef-c-current-line-string ()
  "returns current line as a string"
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

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

(use-package cc-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.hpp$" . c++-mode))
  :config
  (define-key c-mode-map (kbd "C-M-l") nil)
  (define-key c-mode-map (kbd "C-M-h") nil)
  (define-key c++-mode-map (kbd "C-M-l") nil)
  (define-key c++-mode-map (kbd "C-M-h") nil)

  (ef-add-hook c-mode-hook
    (setq-local c-default-style "k&r")
    (setq-local c-basic-offset 8)
    (setq-local tab-width 8)
    (setq-local indent-tabs-mode t))

  (ef-add-hook c++-mode-hook
    (setq-local c-default-style "k&r")
    (setq-local c-basic-offset 2)
    (setq-local tab-width 2)
    (setq-local indent-tabs-mode nil))

  ;; https://stackoverflow.com/questions/23553881/emacs-indenting-of-c11-lambda-functions-cc-mode
  (defadvice c-lineup-arglist (around my activate)
    "Improve indentation of continued C++11 lambda function opened as argument."
    (setq ad-return-value
          (if (and (equal major-mode 'c++-mode)
                   (ignore-errors
                     (save-excursion
                       (goto-char (c-langelem-pos langelem))
                       ;; Detect "[...](" or "[...]{". preceded by "," or "(",
                       ;;   and with unclosed brace.
                       (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
              0
            ad-do-it))))

(use-package ccls
  :after cc-mode
  :custom
  (ccls-sem-highlight-method nil)
  :hook
  (c-mode . ef-cc-mode-enable-lsp)
  (c++-mode . ef-cc-mode-enable-lsp)
  (objc-mode . ef-cc-mode-enable-lsp)
  :commands (ef-cc-mode-enable-lsp)
  :config
  (declare-function ef-project-root "core-project")

  (defun ef-cc-mode-enable-lsp ()
    "Conditionally enable LSP integration for cc-mode projects."
    (interactive)
    (when (or (file-exists-p (expand-file-name "compile_commands.json" (ef-project-root)))
              (file-exists-p (expand-file-name ".ccls" (ef-project-root))))
      (ef-enable-lsp-maybe))))

(use-package cmake-mode
  :commands cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

(use-package ruby-style
  :straight nil
  :after (cc-mode)
  :load-path "vendor/")

(ef-deflang c++
  :after (cc-mode)
  :compile
  (lambda ()
    (interactive)
    (compile "make -k"))
  :compile-and-run
  (lambda ()
    (interactive)
    (compile "make -k run" t)))

(provide 'lang-c)
