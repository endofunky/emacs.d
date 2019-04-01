(use-package sh-script
  :defer t
  :mode (("\\.zsh\\'" . sh-mode)
         ("\\.sh\\'" . sh-mode)
         ("\\.bash\\'" . sh-mode)
         ("zlogin\\'" . sh-mode)
         ("zlogout\\'" . sh-mode)
         ("zprofile\\'" . sh-mode)
         ("zshenv\\'" . sh-mode)
         ("zshrc\\'" . sh-mode))
  :init
  (ef-define-repl ef-repl-zsh "*ansi-term*" #'(lambda () (ansi-term (getenv "SHELL"))))
  :custom
  (sh-basic-offset 2)
  (sh-indent-for-case-label 0)
  (sh-indent-for-case-alt '+)
  :config
  (evil-define-key 'normal sh-mode-map ",r" 'ef-repl-zsh))

(provide 'lang-shell)
