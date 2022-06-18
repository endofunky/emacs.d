(require 'core-evil)
(require 'core-shackle)

(defvar calculate-lisp-indent-last-sexp)

(use-package elisp-mode
  :straight nil
  :commands (emacs-lisp-mode lisp-interaction-mode)
  :functions (ef-emacs-lisp-indent-function
              ef-elisp-describe-thing-at-point)
  :general
  (:keymaps '(emacs-lisp-mode-map
              lisp-interaction-mode-map)
   "C-c C-d d"   'ef-elisp-describe-thing-at-point
   "C-c C-d C-d" 'ef-elisp-describe-thing-at-point)
  (:states 'normal :keymaps '(emacs-lisp-mode-map
                              lisp-interaction-mode-map
                              ielm-map)
   "K" 'ef-elisp-describe-thing-at-point)
  :config
  (defun ef-elisp-describe-thing-at-point ()
    "Display the full documentation of the elisp thing at point.
The named subject may be a function, variable, library or face."
    (interactive)
    (if-let* ((sym-at-point (symbol-at-point))
              (sym-name (and sym-at-point (symbol-name sym-at-point))))
        (if (fboundp 'describe-symbol)
            (describe-symbol (intern sym-name))
          (with-no-warnings
            (help-xref-interned (intern sym-name))))))

  (defun ef-emacs-lisp-indent-function (indent-point state)
    "A replacement for `lisp-indent-function'.

Indents plists more sensibly. Adapted from
https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned"
    (let ((normal-indent (current-column))
          (orig-point (point))
          ;; TODO Refactor `target' usage (ew!)
          target)
      (goto-char (1+ (elt state 1)))
      (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
      (cond ((and (elt state 2)
                  (or (not (looking-at-p "\\sw\\|\\s_"))
                      (eq (char-after) ?:)))
             (unless (> (save-excursion (forward-line 1) (point))
                        calculate-lisp-indent-last-sexp)
               (goto-char calculate-lisp-indent-last-sexp)
               (beginning-of-line)
               (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t))
             (backward-prefix-chars)
             (current-column))
            ((and (save-excursion
                    (goto-char indent-point)
                    (skip-syntax-forward " ")
                    (not (eq (char-after) ?:)))
                  (save-excursion
                    (goto-char orig-point)
                    (and (eq (char-after) ?:)
                         (eq (char-before) ?\()
                         (setq target (current-column)))))
             (save-excursion
               (move-to-column target t)
               target))
            ((let* ((function (buffer-substring (point) (progn (forward-sexp 1) (point))))
                    (method (or (function-get (intern-soft function) 'lisp-indent-function)
                                (get (intern-soft function) 'lisp-indent-hook))))
               (cond ((or (eq method 'defun)
                          (and (null method)
                               (> (length function) 3)
                               (string-match-p "\\`def" function)))
                      (lisp-indent-defform state indent-point))
                     ((integerp method)
                      (lisp-indent-specform method state indent-point normal-indent))
                     (method
                      (funcall method indent-point state))))))))

  (defun ef-elisp-eval-project ()
    (interactive)
    (dolist (buf (buffer-list))
      (let ((project-root (project-root (project-current t)))
            (buffer-major-mode (with-current-buffer buf
                                 major-mode))
            (buffer-file (buffer-file-name buf)))
        (when (and (or (string= buffer-major-mode "emacs-lisp-mode")
                       (string= buffer-major-mode "lisp-interaction-mode"))
                   ;; Skip files not part of the current project
                   (string-prefix-p project-root buffer-file)
                   ;; Skip special files (such as ./tramp)
                   (string-suffix-p ".el" buffer-file)
                   ;; Skip elpa directory
                   (not (string-prefix-p (expand-file-name "elpa" project-root)
                                         buffer-file)))
          (message "Evaluating: %s" buffer-file)
          (eval-buffer buf)))))

  (defun ef-emacs-lisp-recompile ()
    "Recompile elc file corresponding to `buffer-file-name', if it exists."
    (interactive)
    (when (file-exists-p (byte-compile-dest-file buffer-file-name))
      (emacs-lisp-byte-compile)))

  (ef-add-hook emacs-lisp-mode-hook
    (add-hook 'after-save-hook 'ef-emacs-lisp-recompile nil t)
    (setq-local lisp-indent-function #'ef-emacs-lisp-indent-function)
    (setq-local mode-name "E-λ")))

(use-package elisp-def
  :after elisp-mode
  :commands (elisp-def elisp-def-mode)
  :hook
  (ielm-mode . elisp-def-mode)
  (emacs-lisp-mode . elisp-def-mode)
  (lisp-interaction-mode . elisp-def-mode))

(use-package macrostep
  :after elisp-mode)

(use-package package-lint
  :defer t
  :commands package-lint-current-buffer
  :config
  (ef-add-popup "*Package-Lint*"))

(use-package eldoc
  :defer t
  :custom
  (eldoc-idle-delay 0.5)
  (eldoc-echo-area-use-multiline-p 1)
  :config
  ;; Eldoc massively slows down cursor movement. This advice fixes that.
  (advice-add 'eldoc-pre-command-refresh-echo-area :override #'ignore))

(use-package ielm
  :defer t
  :commands (ielm)
  :hook
  (ielm-mode . eldoc-mode)
  :config
  (ef-add-popup "*ielm*"))

(defun ef-ielm-insert-region (start end)
  "Insert the curent region in the IELM REPL buffer."
  (interactive "rP")
  (if-let ((buf (get-buffer "*ielm*")))
      (progn
        (let ((expr (buffer-substring-no-properties start end)))
          (with-current-buffer buf
            (insert expr)))
        (pop-to-buffer buf))
    (user-error "No IELM buffer found")))

(defun ef-ielm-insert-defun ()
  "Insert the top level form at point in the IELM REPL buffer."
  (interactive)
  (if-let ((buf (get-buffer "*ielm*")))
      (progn
        (save-excursion
          (end-of-defun)
          (let ((end (point)))
            (beginning-of-defun)
            (let ((expr (buffer-substring-no-properties (point) end)))
              (with-current-buffer buf
                (insert expr)))))
        (pop-to-buffer buf))
    (user-error "No IELM buffer found")))

(defun ef-ielm-insert-sexp ()
  "Insert the expression preceding point in the IELM REPL buffer."
  (interactive)
  (if-let ((buf (get-buffer "*ielm*")))
      (progn
        (save-excursion
          (thing-at-point--end-of-sexp)
          (let ((end (point)))
            (thing-at-point--beginning-of-sexp)
            (let ((expr (buffer-substring-no-properties (point) end)))
              (with-current-buffer buf
                (insert expr)))))
        (pop-to-buffer buf))
    (user-error "No IELM buffer found")))

(ef-deflang emacs-lisp
  :after elisp-mode
  :maps (emacs-lisp-mode-map lisp-interaction-mode-map)

  ;; compile
  :compile emacs-lisp-byte-compile-and-load
  :compile-nav-jump elisp-def
  :compile-nav-pop-back pop-tag-mark

  ;; doc
  :doc-apropos consult-apropos
  :doc-point ef-elisp-describe-thing-at-point
  :doc-search elisp-index-search

  ;; eval
  :eval-all ef-elisp-eval-project
  :eval-buffer eval-buffer
  :eval-expression eval-expression
  :eval-region eval-region
  :eval-sexp eval-print-last-sexp
  :eval-insert-defun ef-ielm-insert-defun
  :eval-insert-region ef-ielm-insert-region
  :eval-insert-sexp ef-ielm-insert-sexp

  ;; lint
  :lint-file package-lint-current-buffer

  ;; macro
  :macro-expand-all macrostep-expand
  :macro-quit macrostep-collapse-all

  ;; repl
  :repl-toggle ielm

  ;; xref
  :xref-apropos xref-find-apropos
  :xref-definitions xref-find-definitions
  :xref-references xref-find-references)

(provide 'lang-elisp)
