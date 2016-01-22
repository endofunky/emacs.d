(use-package sh-script
  :defer t
  :mode (("\\.zsh\\'" . sh-mode)
         ("\\.sh\\'" . sh-mode)
         ("\\.bash\\'" . sh-mode)
         ("zlogin\\'" . sh-mode)
         ("zlogout\\'" . sh-mode)
         ("zprofile\\'" . sh-mode)
         ("zshenv\\'" . sh-mode)
         ("zshrc\\'" . sh-mode)))

(provide 'lang-shell)
