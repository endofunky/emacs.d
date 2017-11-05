(use-package tuareg
  :ensure t
  :mode (("\\.ml[ily]?$" . tuareg-mode)
         ("\\.topml$" . tuareg-mode))
  :config
  (ef-define-repl ef-repl-ocaml "*OCaml*" 'tuareg-run-ocaml)
  (evil-define-key 'normal tuareg-mode-map ",eb" 'tuareg-eval-buffer)
  (evil-define-key 'visual tuareg-mode-map ",er" 'tuareg-eval-region)
  (evil-define-key 'normal tuareg-mode-map ",ep" 'tuareg-eval-phrase)
  (evil-define-key 'normal tuareg-mode-map ",r" 'ef-repl-ocaml)
  (evil-define-key 'normal tuareg-interactive-mode-map ",r" 'ef-repl-ocaml))

(use-package merlin
  :ensure t
  :config
  (setq merlin-command 'opam)
  (add-hook 'tuareg-mode-hook 'merlin-mode t)
  (add-hook 'caml-mode-hook 'merlin-mode t)
  (setq merlin-locate-in-new-window 'never)
  (setq merlin-completion-with-doc t)
  (evil-define-key 'normal tuareg-mode-map ",," 'merlin-locate)
  (evil-define-key 'normal tuareg-mode-map ",." 'merlin-pop-stack))

(use-package ocp-indent
  :ensure t)

(provide 'lang-ocaml)
