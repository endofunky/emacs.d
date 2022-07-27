;;; lang-shell.el --- Shell script configuration -*- lexical-binding: t; -*-
(require 'use-package)

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
  (sh-basic-offset 8)
  (sh-indent-for-case-label 0)
  (sh-indent-for-case-alt '+)
  (sh-indent-after-continuation 'always)
  :general
  (:keymaps 'sh-mode-map :states 'normal
   "K" 'woman))

(use-package flymake-shellcheck
  :defer t
  :after sh-script
  :commands flymake-shellcheck-load
  :hook
  (sh-mode . flymake-shellcheck-load))

(use-package shfmt
  :after sh-script
  :if (executable-find "shfmt")
  :hook
  (sh-mode . shfmt-on-save-mode))

(provide 'lang-shell)
