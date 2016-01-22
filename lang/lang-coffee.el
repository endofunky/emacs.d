(use-package coffee-mode
  :ensure t
  :mode ("\\.coffee\\'" "Cakefile\\'")
  :config
  (defun ts/coffee-mode-hook ()
    (setq coffee-tab-width 2))

  (add-hook 'coffee-mode-hook 'ts/coffee-mode-hook))

(provide 'lang-coffee)
