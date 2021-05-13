(require 'core-evil)
(require 'core-lib)
(require 'core-shackle)
(require 'core-projectile)

(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn$" . clojure-mode))
  :config
  (defun ef-cider-jack-in (params)
    "Quit cider if running and jack in again"
    (interactive "P")
    (when (cider-connected-p)
      (if-let* ((buf (cider-current-repl))
                (win (get-buffer-window buf))
                (_ (window-parent win)))
          (delete-window win))
      (cider-quit))
    (cider-jack-in params)))

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
  :functions (cider-connected-p
              cider-current-repl
              cider-quit)
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
  (:states 'normal :keymaps 'cider-repl-mode-map :prefix ef-prefix
           "r" 'quit-window)
  :config
  (require 'cider-apropos)
  (require 'cider-macroexpansion)
  (require 'cider-ns)

  (ef-shackle '(cider-repl-mode :align bottom :size .4 :popup t :select t)
              '("*cider-test-report*" :align bottom :size .4 :popup t :select t)
              '("*cider-doc*" :align bottom :size .4 :popup t :select t)
              '("*cider-apropos*" :align bottom :size .4 :popup t :select t)
              '("*cider-error*" :align bottom :size .4 :popup t :select t)
              '("*cider-macroexpansion*" :align bottom :size .4 :popup t :select t))

  (defun ef-cider-run-test ()
    "Re-evaluate buffer and run test at point"
    (interactive)
    (cider-load-file
     (projectile-find-implementation-or-test (buffer-file-name)))
    (cider-eval-buffer)
    (cider-test-run-test))

  (defun ef-cider-run-ns-tests ()
    "Re-evaluate buffer and run tests for namespace"
    (interactive)
    (cider-load-file
     (projectile-find-implementation-or-test (buffer-file-name)))
    (cider-eval-buffer)
    (cider-test-run-ns-tests nil))

  (defun ef-cider-run-all-tests ()
    "Re-evaluate buffer and run all tests"
    (interactive)
    (cider-eval-buffer)
    (cider-test-run-project-tests nil))

  (evil-define-key 'normal cider-mode-map ",r" 'cider-switch-to-repl-buffer)
  (evil-define-key 'normal cider-mode-map ",ns" 'cider-repl-set-ns))

(use-package flycheck-clj-kondo
  :after clojure-mode
  :ensure t
  :config
  (require 'flycheck-clj-kondo))

(ef-deflang clojure
  :compile-backend-connect ef-cider-jack-in
  :compile-backend-reconnect ef-cider-jack-in
  :compile-backend-quit cider-quit
  :compile-nav-jump cider-find-var
  :compile-nav-pop-back cider-pop-back
  :doc-apropos cider-apropos
  :doc-apropos-select cider-apropos-select
  :doc-search cider-apropos-documentation
  :eval-buffer cider-eval-buffer
  :eval-all cider-ns-reload-all
  :eval-file cider-eval-file
  :eval-defun cider-eval-defun-at-point
  :eval-project cider-load-all-project-ns
  :eval-region cider-eval-region
  :macro-expand-all cider-macroexpand-all-inplace
  :macro-expand-one cider-macroexpand-1-inplace
  :macro-quit cider-macroexpand-undo
  :test-file ef-cider-run-ns-tests
  :test-at-point ef-cider-run-test
  :test-all ef-cider-run-all-tests
  :test-report cider-test-show-report)

(provide 'lang-clojure)
