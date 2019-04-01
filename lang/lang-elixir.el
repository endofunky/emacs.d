(use-package elixir-mode
  :mode (("\\.ex\\'" . elixir-mode)
         ("\\.exs\\'" . elixir-mode))
  :commands elixir-mode
  :ensure t)

(use-package alchemist
  :ensure t
  :commands (alchemist-mode alchemist-iex-project-run)
  :init
  (add-hook 'elixir-mode-hook 'alchemist-mode)
  :config
  (ef-define-repl ef-repl-alchemist "*Alchemist-IEx*" 'alchemist-iex-project-run)
  (evil-define-key 'normal alchemist-mode-map ",r" 'ef-repl-alchemist)
  (evil-define-key 'normal alchemist-iex-mode-map ",r" 'ef-repl-alchemist)
  (evil-define-key 'normal alchemist-mode-map ",," 'alchemist-goto-definition-at-point)
  (evil-define-key 'normal alchemist-mode-map ",." 'alchemist-goto-jump-back))

(provide 'lang-elixir)
