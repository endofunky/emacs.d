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
  :config
  (setq sh-basic-offset 2)
  (setq sh-indent-for-case-label 0)
  (setq sh-indent-for-case-alt '+))

(provide 'lang-shell)
