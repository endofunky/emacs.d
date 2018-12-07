(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn$" . clojure-mode))
  :config
  (setq clojure--prettify-symbols-alist
        '(("fn" . ?λ)))

  (add-hook 'clojure-mode-hook 'evil-cleverparens-mode)
  (evil-define-key 'normal clojure-mode-map ",cjq" 'cider-quit)
  (evil-define-key 'normal clojure-mode-map ",cji" 'cider-jack-in))

(use-package cider
  :ensure t
  :config
  (setq cider-prompt-for-symbol nil
        cider-repl-display-help-banner nil
        cider-repl-use-pretty-printing t)

  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)

  (ef-shackle '(cider-repl-mode :align bottom :size .4 :popup t :select t)
              '("*cider-error*" :align bottom :size .4 :popup t :select t))

  (evil-define-key 'normal cider-mode-map ",," 'cider-find-var)
  (evil-define-key 'normal cider-mode-map ",." 'cider-pop-back)
  (evil-define-key 'normal cider-mode-map ",eb" 'cider-eval-buffer)
  (evil-define-key 'normal cider-mode-map ",ef" 'cider-eval-file)
  (evil-define-key 'normal cider-mode-map ",ed" 'cider-eval-defun-at-point)
  (evil-define-key 'visual cider-mode-map ",er" 'cider-eval-region)
  (evil-define-key 'normal cider-mode-map ",r" 'cider-switch-to-repl-buffer)
  (evil-define-key 'normal cider-mode-map ",ns" 'cider-repl-set-ns)
  (evil-define-key 'normal cider-stacktrace-mode-map "q" 'cider-popup-buffer-quit-function)

  (evil-define-key 'normal cider-repl-mode-map ",r" 'quit-window)
  (evil-define-key 'insert cider-repl-mode-map (kbd "<up>") 'cider-repl-previous-input)
  (evil-define-key 'insert cider-repl-mode-map (kbd "<down>") 'cider-repl-next-input))

(use-package clj-refactor
  :ensure t
  :after cider
  :init
  (add-hook 'clojure-mode-hook 'clj-refactor-mode 1))

(provide 'lang-clojure)
