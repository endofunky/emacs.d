(use-package elixir-mode
  :mode (("\\.ex\\'" . elixir-mode)
         ("\\.exs\\'" . elixir-mode))
  :commands elixir-mode
  :defer t
  :ensure t)

(use-package alchemist
  :ensure t
  :defer t
  :commands (alchemist-mode alchemist-iex-project-run)
  :init
  (add-hook 'elixir-mode-hook 'alchemist-mode)
  :config
  (ts/define-repl ts/repl-alchemist "*Alchemist-IEx*" 'alchemist-iex-project-run)
  (evil-define-key 'normal alchemist-mode-map ",r" 'ts/repl-alchemist)
  (evil-define-key 'normal alchemist-iex-mode-map ",r" 'ts/repl-alchemist)
  (evil-define-key 'normal alchemist-mode-map ",," 'alchemist-goto-definition-at-point)
  (evil-define-key 'normal alchemist-mode-map ",." 'alchemist-goto-jump-back))

(provide 'lang-elixir)
