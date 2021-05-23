(require 'core-evil)
(require 'core-lib)
(require 'core-shackle)
(require 'core-projectile)

(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn$" . clojure-mode)))

(use-package cider
  :ensure t
  :custom
  (cider-print-fn 'fipp)
  (cider-prompt-for-project-on-connect nil)
  (cider-prompt-for-symbol nil)
  (cider-repl-display-help-banner nil)
  (cider-repl-use-pretty-printing t)
  (nrepl-hide-special-buffers t)
  (nrepl-log-message nil)
  :hook
  (clojure-mode . cider-mode)
  (cider-mode . eldoc-mode)
  (cider-repl-mode . eldoc-mode)
  :functions (ef-cider-quit)
  :commands (cider-load-file
             cider-load-buffer
             cider-test-run-test
             cider-test-run-ns-tests
             cider-test-run-project-tests)
  :general
  (:states '(normal) :keymaps '(cider-test-report-mode-map
                                cider-stacktrace-mode-map
                                cider-macroexpansion-mode-map)

   "q" 'cider-popup-buffer-quit-function)
  (:states 'insert :keymaps 'cider-repl-mode-map
   "<down>" 'cider-repl-next-input
   "<up>" 'cider-repl-previous-input)
  (:states 'normal :keymaps 'cider-browse-spec-view-mode-map
   "^" 'cider-browse-spec-all
   "e" 'cider-browse-spec--print-curr-spec-example
   "n" 'forward-button
   "p" 'backward-button)
  :config
  (require 'cider-ns)
  (require 'cider-tracing)

  (declare-function cider-connected-p "cider")
  (declare-function cider-jack-in "cider")
  (declare-function cider-current-repl "cider")
  (declare-function cider-quit "cider")

  (ef-add-popup 'cider-repl-mode)
  (ef-add-popup "*cider-repl-history*" :ephemeral t)
  (ef-add-popup "*cider-test-report*" :ephemeral t)
  (ef-add-popup "*cider-doc*" :ephemeral t)
  (ef-add-popup "*cider-apropos*" :ephemeral t)
  (ef-add-popup "*cider-error*" :ephemeral t)
  (ef-add-popup "*cider-spec-browser*" :ephemeral t)
  (ef-add-popup "*cider-spec-example*" :ephemeral t)
  (ef-add-popup "*cider-macroexpansion*" :ephemeral t)
  (ef-add-popup "*cider-inspect*" :ephemeral t)
  (ef-add-popup "*CIDER REPL Shortcuts Help*" :ephemeral t :size .5)

  (defun ef-cider-jack-in (params)
    "Quit CIDER if running and jack in again"
    (interactive "P")
    (ef-cider-quit)
    (cider-jack-in params))

  (defun ef-cider-quit ()
    "Quit CIDER"
    (interactive)
    (when (cider-connected-p)
      (if-let* ((buf (cider-current-repl))
                (win (get-buffer-window buf))
                (_ (window-parent win)))
          (delete-window win))
      (cider-quit)))

  (defun ef-cider-run-test ()
    "Re-evaluate buffer and run test at point"
    (interactive)
    (cider-load-buffer)
    (call-interactively #'cider-test-run-test))

  (defun ef-cider-run-ns-tests ()
    "Re-evaluate buffer and run tests for namespace"
    (interactive)
    (cider-load-buffer)
    (call-interactively #'cider-test-run-ns-tests))

  (defun ef-cider-test-rerun-failed-tests ()
    "Re-evaluate buffer and re-run all failed tests"
    (interactive)
    (cider-load-buffer)
    (call-interactively #'cider-test-rerun-failed-tests))

  (defun ef-cider-run-all-tests ()
    "Re-evaluate buffer and run all tests"
    (interactive)
    (cider-test-run-project-tests nil)))

(use-package cider-apropos
  :after cider
  :commands (cider-apropos
             cider-apropos-select
             cider-apropos-documentation))

(use-package cider-xref
  :commands (cider-xref-fn-deps-select
             cider-xref-fn-refs-select))

(use-package macrostep-geiser
  :after cider
  :ensure t
  :hook
  (cider-mode . macrostep-geiser-setup))

(use-package flycheck-clj-kondo
  :after clojure-mode
  :ensure t
  :config
  (require 'flycheck-clj-kondo))

(ef-deflang clojure
  :after (clojure-mode cider)

  ;; compile
  :compile-backend-connect ef-cider-jack-in
  :compile-backend-reconnect ef-cider-jack-in
  :compile-backend-quit ef-cider-quit
  :compile-inspect cider-inspect
  :compile-nav-jump cider-find-var
  :compile-nav-pop-back cider-pop-back

  ;; doc
  :doc-apropos cider-apropos
  :doc-apropos-select cider-apropos-select
  :doc-cheatsheet clojure-view-cheatsheet
  :doc-guide clojure-view-guide
  :doc-manual clojure-view-reference-section
  :doc-search cider-apropos-documentation

  ;; eval
  :eval-buffer cider-eval-buffer
  :eval-all cider-ns-reload-all
  :eval-file cider-eval-file
  :eval-defun cider-eval-defun-at-point
  :eval-region cider-eval-region
  :eval-sexp cider-eval-last-sexp
  :eval-insert-defun cider-insert-defun-in-repl
  :eval-insert-region cider-insert-region-in-repl
  :eval-insert-sexp cider-insert-last-sexp-in-repl

  ;; macro
  :macro-expand-all macrostep-expand
  :macro-quit cider-macroexpand-undo

  ;; repl
  :repl-context cider-repl-set-ns
  :repl-toggle cider-switch-to-repl-buffer
  :repl-quit cider-quit

  ;; specification
  :specification-browse cider-browse-spec
  :specification-browse cider-browse-spec-all

  ;; test
  :test-errors ef-cider-test-rerun-failed-tests
  :test-file ef-cider-run-ns-tests
  :test-at-point ef-cider-run-test
  :test-all ef-cider-run-all-tests
  :test-toggle projectile-toggle-between-implementation-and-test
  :test-report cider-test-show-report

  ;; trace
  :trace-variable cider-toggle-trace-var
  ;; xref
  :xref-references cider-xref-fn-refs-select
  :xref-dependencies cider-xref-fn-deps-select)

(provide 'lang-clojure)
