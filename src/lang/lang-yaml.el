(use-package yaml-mode
  :defer t
  :mode (("\\.ya?ml\\'" . yaml-mode)
         ("\\.ya?ml.dist\\'" . yaml-mode)
         ("\\.ya?ml.erb\\'" . yaml-mode)))

(provide 'lang-yaml)
