(require 'core-evil)

(use-package sh-script
  :defer t
  :mode (("\\.zsh\\'" . sh-mode)
         ("\\.sh\\'" . sh-mode)
         ("\\.bash\\'" . sh-mode)
         ("\\.?zlogin\\'" . sh-mode)
         ("\\.?zlogout\\'" . sh-mode)
         ("\\.?zprofile\\'" . sh-mode)
         ("\\.?zshenv\\'" . sh-mode)
         ("\\.?zshrc\\'" . sh-mode)
         ("\\.?xinit\\'" . sh-mode)
         ("\\.?xprofile\\'" . sh-mode)
         ("\\.?xsession\\'" . sh-mode))
  :custom
  (sh-basic-offset 2)
  (sh-indent-for-case-label 0)
  (sh-indent-for-case-alt '+)
  :config
  (evil-define-key 'normal sh-mode-map ",r" 'anti-term))

(use-package company-shell
  :after sh-script
  :custom
  (company-shell-clean-manpage t)
  :config
  (add-to-list 'company-backends 'company-shell))

(provide 'lang-shell)
