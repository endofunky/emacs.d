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
             cider-eval-buffer
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
  :config
  (require 'cider-ns)

  (declare-function cider-connected-p "cider")
  (declare-function cider-jack-in "cider")
  (declare-function cider-current-repl "cider")
  (declare-function cider-quit "cider")

  (ef-shackle '(cider-repl-mode :align bottom :size .4 :popup t :select t)
              '("*cider-test-report*" :align bottom :size .4 :popup t :select t)
              '("*cider-doc*" :align bottom :size .4 :popup t :select t)
              '("*cider-apropos*" :align bottom :size .4 :popup t :select t)
              '("*cider-error*" :align bottom :size .4 :popup t :select t)
              '("*cider-spec-browser*" :align bottom :size .4 :popup t :select t)
              '("*cider-macroexpansion*" :align bottom :size .4 :popup t :select t))

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
    (with-demoted-errors "Error: %S"
      (cider-load-file
       (projectile-find-implementation-or-test (buffer-file-name))))
    (cider-eval-buffer)
    (cider-test-run-test))

  (defun ef-cider-run-ns-tests ()
    "Re-evaluate buffer and run tests for namespace"
    (interactive)
    (with-demoted-errors "Error: %S"
      (cider-load-file
       (projectile-find-implementation-or-test (buffer-file-name))))
    (cider-eval-buffer)
    (cider-test-run-ns-tests nil))

  (defun ef-cider-test-rerun-failed-tests ()
    "Re-evaluate buffer and re-run all failed tests"
    (interactive)
    (with-demoted-errors "Error: %S"
      (cider-load-file
       (projectile-find-implementation-or-test (buffer-file-name))))
    (cider-eval-buffer)
    (cider-test-run-test))

  (defun ef-cider-run-all-tests ()
    "Re-evaluate buffer and run all tests"
    (interactive)
    (with-demoted-errors "Error: %S"
      (cider-load-file
       (projectile-find-implementation-or-test (buffer-file-name))))
    (cider-test-run-project-tests nil)))

(use-package cider-apropos
  :after cider
  :commands (cider-apropos
             cider-apropos-select
             cider-apropos-documentation))

(use-package cider-macroexpansion
  :after cider
  :commands (cider-macroexpand-all-inplace
             cider-macroexpand-1-inplace
             cider-macroexpand-undo))

(use-package cider-xref
  :commands (cider-xref-fn-deps-select
             cider-xref-fn-refs-select))

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
  :eval-project cider-load-all-project-ns
  :eval-region cider-eval-region

  ;; macro
  :macro-expand-all cider-macroexpand-all-inplace
  :macro-expand-one cider-macroexpand-1-inplace
  :macro-quit cider-macroexpand-undo

  ;; repl
  :repl-context cider-repl-set-ns
  :repl-toggle cider-switch-to-repl-buffer
  :repl-quit cider-quit

  ;; test
  :test-errors ef-cider-test-rerun-failed-tests
  :test-file ef-cider-run-ns-tests
  :test-at-point ef-cider-run-test
  :test-all ef-cider-run-all-tests
  :test-toggle projectile-toggle-between-implementation-and-test
  :test-report cider-test-show-report

  ;; xref
  :xref-definitions cider-xref-fn-refs-select
  :xref-dependencies cider-xref-fn-deps-select)

(provide 'lang-clojure)
