(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn$" . clojure-mode))
  :config
  (setq clojure--prettify-symbols-alist
        '(("fn" . ?λ)))

  (defun ef-cider-jack-in (params)
    "Quit cider if running and jack in again"
    (interactive "P")
    (if (cider-connected-p)
      (cider-quit))
    (cider-jack-in params))

  (evil-define-key 'normal clojure-mode-map ",cjq" 'cider-quit)
  (evil-define-key 'normal clojure-mode-map ",cji" 'ef-cider-jack-in))

(use-package cider
  :ensure t
  :commands (cider-connected-p cider-quit cider-jack-in)
  :custom
  (cider-prompt-for-symbol nil)
  (cider-repl-display-help-banner nil)
  (cider-repl-use-pretty-printing t)
  (nrepl-hide-special-buffers t)
  (nrepl-log-message nil)
  :config
  (require 'cider-ns)

  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)

  (ef-shackle '(cider-repl-mode :align bottom :size .4 :popup t :select t)
              '("*cider-test-report*" :align bottom :size .4 :popup t :select t)
              '("*cider-error*" :align bottom :size .4 :popup t :select t))

  (defun ef-cider-run-test ()
    "Re-evaluate buffer and run test at point"
    (interactive)
    (cider-eval-buffer)
    (cider-ns-reload-all)
    (cider-test-run-test))

  (defun ef-cider-run-ns-tests ()
    "Re-evaluate buffer and run tests for namespace"
    (interactive)
    (cider-eval-buffer)
    (cider-ns-reload-all)
    (cider-test-run-ns-tests nil))

  (defun ef-cider-run-all-tests ()
    "Re-evaluate buffer and run tests for namespace"
    (interactive)
    (cider-eval-buffer)
    (cider-ns-reload-all)
    (cider-test-run-project-tests nil))

  (evil-define-key 'normal cider-mode-map ",," 'cider-find-var)
  (evil-define-key 'normal cider-mode-map ",." 'cider-pop-back)
  (evil-define-key 'normal cider-mode-map ",eb" 'cider-eval-buffer)
  (evil-define-key 'normal cider-mode-map ",ef" 'cider-eval-file)
  (evil-define-key 'normal cider-mode-map ",ed" 'cider-eval-defun-at-point)
  (evil-define-key 'visual cider-mode-map ",er" 'cider-eval-region)
  (evil-define-key 'normal cider-mode-map ",r" 'cider-switch-to-repl-buffer)
  (evil-define-key 'normal cider-mode-map ",ns" 'cider-repl-set-ns)
  (evil-define-key 'normal cider-mode-map ",tt" 'ef-cider-run-ns-tests)
  (evil-define-key 'normal cider-mode-map ",tp" 'ef-cider-run-test)
  (evil-define-key 'normal cider-mode-map ",ta" 'ef-cider-run-all-tests)
  (evil-define-key 'normal cider-test-report-mode-map "q" 'cider-popup-buffer-quit-function)
  (evil-define-key 'normal cider-stacktrace-mode-map "q" 'cider-popup-buffer-quit-function)

  (evil-define-key 'normal cider-repl-mode-map ",r" 'quit-window)
  (evil-define-key 'insert cider-repl-mode-map (kbd "<up>") 'cider-repl-previous-input)
  (evil-define-key 'insert cider-repl-mode-map (kbd "<down>") 'cider-repl-next-input))

(use-package flycheck-clj-kondo
  :after clojure-mode
  :ensure t
  :config
  (require 'flycheck-clj-kondo))

(provide 'lang-clojure)
