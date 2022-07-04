;;; lang-elisp.el --- Emacs Lisp configuration -*- lexical-binding: t; -*-
(require 'core-evil)
(require 'core-shackle)

(defvar calculate-lisp-indent-last-sexp)

(use-package elisp-mode
  :straight nil
  :functions (+emacs-lisp-indent-function
              +elisp-describe-thing-at-point)
  :general
  (:prefix ef-local-leader :states 'visual :keymaps '(emacs-lisp-mode-map
                                                      lisp-interaction-mode-map)
   "e"   '(nil :wk "Eval")
   "ei"  '(nil :wk "IELM insert")
   "eir" '(+ielm-insert-region :wk "Region")
   "er"  '(eval-region :wk "Region"))
  (:prefix ef-local-leader :states 'normal :keymaps '(emacs-lisp-mode-map
                                                      lisp-interaction-mode-map)
   "c"   '(nil :wk "Compile")
   "cc"  '(emacs-lisp-byte-compile-and-load :wk "Byte-compile & load")

   "e"   '(nil :wk "Eval")
   "ea"  '(+elisp-eval-project :wk "Project")
   "eb"  '(eval-buffer :wk "Buffer")
   "ee"  '(eval-expression :wk "Expression")
   "ei"  '(nil :wk "IELM insert")
   "eid" '(+ielm-insert-defun :wk "Defun")
   "eis" '(+ielm-insert-sexp :wk "S-exp")
   "es"  '(eval-sexp :wk "S-exp")

   "l"   '(nil :wk "Lint")
   "lb"  '(package-lint-current-buffer :wk "Buffer")

   "m"   '(nil :wk "Macro")
   "me"  '(macrostep-expand :wk "Expand")
   "mq"  '(macrostep-collapse-all :wk "Quit")

   "r"   '(nil :wk "REPL")
   "rr"  '(ielm :wk "Open"))
  (:states 'normal :keymaps '(emacs-lisp-mode-map
                              lisp-interaction-mode-map
                              ielm-map)
   "K" '+elisp-describe-thing-at-point)
  :config
  (defun +elisp-describe-thing-at-point ()
    "Display the full documentation of the elisp thing at point.
The named subject may be a function, variable, library or face."
    (interactive)
    (if-let* ((sym-at-point (symbol-at-point))
              (sym-name (and sym-at-point (symbol-name sym-at-point))))
        (if (fboundp 'describe-symbol)
            (describe-symbol (intern sym-name))
          (with-no-warnings
            (help-xref-interned (intern sym-name))))))

  (defun +emacs-lisp-indent-function (indent-point state)
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
            ((let* ((function (buffer-substring (point)
                                                (progn (forward-sexp 1)
                                                       (point))))
                    (method (or (function-get (intern-soft function)
                                              'lisp-indent-function)
                                (get (intern-soft function)
                                     'lisp-indent-hook))))
               (cond ((or (eq method 'defun)
                          (and (null method)
                               (> (length function) 3)
                               (string-match-p "\\`def" function)))
                      (lisp-indent-defform state indent-point))
                     ((integerp method)
                      (lisp-indent-specform
                       method state indent-point normal-indent))
                     (method
                      (funcall method indent-point state))))))))

  (defun +elisp-eval-project ()
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

  (defun +emacs-lisp-recompile ()
    "Recompile elc file corresponding to `buffer-file-name', if it exists."
    (interactive)
    (when (file-exists-p (byte-compile-dest-file buffer-file-name))
      (emacs-lisp-byte-compile)))

  (+add-hook emacs-lisp-mode-hook
    (add-hook 'after-save-hook '+emacs-lisp-recompile nil t)
    (setq-local lisp-indent-function #'+emacs-lisp-indent-function)
    (setq-local mode-name "E-λ")))

(use-package elisp-def
  :after elisp-mode
  :general
  (:states '(normal visual) :keymaps 'elisp-def-mode-map
   "gd"  'elisp-def
   "C-t" 'xref-go-back)
  :commands (elisp-def elisp-def-mode)
  :hook
  (ielm-mode . elisp-def-mode)
  (emacs-lisp-mode . elisp-def-mode)
  (lisp-interaction-mode . elisp-def-mode))

(use-package macrostep
  :defer t
  :commands (macrostep-expand
             macrostep-collapse-all))

(use-package package-lint
  :defer t
  :commands package-lint-current-buffer
  :config
  (poe-popup "*Package-Lint*"))

(use-package ielm
  :defer t
  :commands (ielm)
  :hook
  (ielm-mode . eldoc-mode)
  :config
  (poe-popup "*ielm*"))

(defun +ielm-insert-region (start end)
  "Insert the curent region in the IELM REPL buffer."
  (interactive "rP")
  (if-let ((buf (get-buffer "*ielm*")))
      (progn
        (let ((expr (buffer-substring-no-properties start end)))
          (with-current-buffer buf
            (insert expr)))
        (pop-to-buffer buf))
    (user-error "No IELM buffer found")))

(defun +ielm-insert-defun ()
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

(defun +ielm-insert-sexp ()
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

(provide 'lang-elisp)
