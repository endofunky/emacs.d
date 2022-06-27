(require 'core-evil)
(require 'core-shackle)

(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn$" . clojure-mode)))

(use-package cider
  :custom
  (cider-print-fn 'fipp)
  (cider-prompt-for-project-on-connect nil)
  (cider-prompt-for-symbol nil)
  (cider-repl-display-help-banner nil)
  (cider-repl-use-pretty-printing t)
  (cider-show-error-buffer 'except-in-repl)
  (nrepl-hide-special-buffers t)
  (nrepl-log-message nil)
  :hook
  (clojure-mode . cider-mode)
  (cider-mode . eldoc-mode)
  (cider-repl-mode . eldoc-mode)
  (cider-repl-mode . lispy-mode)
  :functions (ef-cider-quit
              cider-test-rerun-failed-tests
              cider-connected-p
              cider-jack-in
              cider-current-repl
              cider-quit)
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
  (require 'cider-format)
  (require 'cider-ns)
  (require 'cider-tracing)

  (evil-set-initial-state 'cider-repl-mode 'normal)

  (ef-add-popup 'cider-repl-mode)
  (ef-add-popup "^\\*cider" :ephemeral t :regexp t)
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
  :straight nil
  :after cider
  :commands (cider-apropos
             cider-apropos-select
             cider-apropos-documentation))

(use-package cider-xref
  :straight nil
  :commands (cider-xref-fn-deps-select
             cider-xref-fn-refs-select))

;; The name would suggest that this is a macrostep implementation for Scheme,
;; but it supports CIDER, too.
(use-package macrostep-geiser
  :after cider
  :hook
  (cider-mode . macrostep-geiser-setup))

(provide 'lang-clojure)
