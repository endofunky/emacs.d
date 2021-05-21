(require 'core-lib)
(require 'core-shackle)

(defvar inferior-lisp-program "sbcl")

(use-package lisp-mode
  :mode (("\\.cl\\'" . lisp-mode)
         ("\\.lisp\\'" . lisp-mode)
         ("\\.sbclrc\\'" . lisp-mode))
  :config
  (ef-add-hook lisp-mode-hook
    (setq-local mode-name "λ")))

(use-package sly
  :after lisp-mode
  :ensure t
  :commands (sly sly-mrepl)
  :custom
  (sly-net-coding-system 'utf-8-unix)
  (sly-auto-start 'ask)
  :general
  (:states 'normal :keymaps 'sly-mode-map
           "K" 'sly-describe-symbol)
  (:states 'insert :keymaps 'sly-mrepl-mode-map
           "<down>" 'sly-mrepl-next-input-or-button
           "<up>" 'sly-mrepl-previous-input-or-button)
  :hook
  (lisp-mode . sly-mode)
  :config
  (ef-add-popup "^\\*sly-mrepl" :regexp t)
  (ef-add-popup "^\\*sly-compilation" :regexp t :ephemeral t)
  (ef-add-popup "^\\*sly-traces" :regexp t :ephemeral t)
  (ef-add-popup "^\\*sly-inspector" :regexp t :ephemeral t)
  (ef-add-popup "^\\*sly-db" :regexp t :ephemeral t)
  (ef-add-popup "^\\*sly-description" :regexp t :ephemeral t))

(use-package sly-macrostep
  :ensure t
  :after sly)

(use-package sly-quicklisp
  :ensure t
  :after sly)

(ef-deflang lisp
  :compile sly-compile-and-load-file
  :compile-defun sly-compile-defun
  :compile-disassemble sly-disassemble-symbol
  :compile-inspect sly-inspect
  :compile-region sly-compile-region
  :compile-backend-connect sly
  :compile-nav-jump sly-edit-definition
  :compile-nav-pop-back sly-pop-find-definition-stack

  :repl-toggle
  (lambda ()
    (interactive)
    (sly-mrepl #'display-buffer))

  :doc-apropos sly-apropos
  :doc-point sly-describe-symbol
  :doc-search sly-hyperspec-lookup

  :eval-buffer sly-eval-buffer
  :eval-sexp sly-eval-last-expression
  :eval-defun sly-eval-defun
  :eval-undefine sly-undefine-function
  :eval-region sly-eval-region

  :macro-expand-all sly-macroexpand-all-inplace
  :macro-expand-one sly-macroexpand-1-inplace
  :macro-quit sly-macroexpand-undo

  :package-add sly-quickload

  :xref-references sly-who-calls
  :xref-dependencies sly-calls-who)


(provide 'lang-common-lisp)
