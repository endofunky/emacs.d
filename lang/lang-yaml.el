(use-package yaml-mode
  :ensure t
  :defer t
  :mode (("\\.ya?ml\\'" . yaml-mode)
         ("\\.ya?ml.erb\\'" . yaml-mode)))

(provide 'lang-yaml)
