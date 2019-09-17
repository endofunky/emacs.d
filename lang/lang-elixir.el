(use-package elixir-mode
  :mode (("\\.ex\\'" . elixir-mode)
         ("\\.exs\\'" . elixir-mode))
  :commands elixir-mode
  :ensure t
  :config
  (add-hook 'elixir-mode-hook #'lsp))

(use-package alchemist
  :ensure t
  :commands (alchemist-mode alchemist-iex-project-run)
  :init
  (add-hook 'elixir-mode-hook 'alchemist-mode)
  :config
  (ef-shackle
   '("*alchemist help*" :align bottom :size .4 :popup t :select t)
   '("*alchemist-refcard*" :align bottom :size .4 :popup t :select t)
   '("*alchemist test report*" :align bottom :size .4 :popup t :select t))

  (ef-define-repl ef-repl-alchemist "*Alchemist-IEx*" 'alchemist-iex-project-run)
  (evil-define-key 'normal alchemist-mode-map ",m" 'alchemist-help)
  (evil-define-key 'normal alchemist-mode-map ",cc" 'alchemist-compile)
  (evil-define-key 'normal alchemist-mode-map ",eb" 'alchemist-iex-compile-this-buffer-and-go)
  (evil-define-key 'visual alchemist-mode-map ",er" 'alchemist-iex-send-region)
  (evil-define-key 'normal alchemist-mode-map ",r" 'ef-repl-alchemist)
  (evil-define-key 'normal alchemist-mode-map ",tp" 'alchemist-mix-test-at-point)
  (evil-define-key 'normal alchemist-mode-map ",tt" 'alchemist-project-run-tests-for-current-file)
  (evil-define-key 'normal alchemist-iex-mode-map ",r" 'ef-repl-alchemist))

(use-package flycheck-mix
  :ensure t
  :commands flycheck-mix-setup
  :init (add-hook 'elixir-mode-hook 'flycheck-mix-setup))

(provide 'lang-elixir)
