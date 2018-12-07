(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode))
  :config
  (add-hook 'clojure-mode-hook 'evil-cleverparens-mode)
  (evil-define-key 'normal clojure-mode-map ",cji" 'cider-jack-in))
(evil-define-key 'normal clojure-mode-map ",cjq" 'cider-quit)

(use-package cider
  :ensure t
  :after clojure-mode
  :config
  (setq cider-prompt-for-symbol nil)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (evil-define-key 'normal cider-mode-map ",," 'cider-find-var)
  (evil-define-key 'normal cider-mode-map ",." 'cider-pop-back)
  (evil-define-key 'normal cider-mode-map ",eb" 'cider-eval-buffer)
  (evil-define-key 'normal cider-mode-map ",ef" 'cider-eval-file)
  (evil-define-key 'normal cider-mode-map ",ed" 'cider-eval-defun-at-point)
  (evil-define-key 'visual cider-mode-map ",er" 'cider-eval-region)

  (defun ef-close-current-window ()
    "Closes the current window"
    (interactive)
    (delete-window (get-buffer-window)))

  (ef-shackle '(cider-repl-mode :align bottom :size .4 :popup t :select t))
  (evil-define-key 'normal cider-mode-map ",r" 'cider-switch-to-repl-buffer)
  (evil-define-key 'normal cider-repl-mode-map ",r" 'ef-close-current-window)
  (evil-define-key 'insert cider-repl-mode-map (kbd "<up>") 'cider-repl-previous-input)
  (evil-define-key 'insert cider-repl-mode-map (kbd "<down>") 'cider-repl-next-input))

(provide 'lang-clojure)
