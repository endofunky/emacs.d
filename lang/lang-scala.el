(use-package scala-mode2
  :ensure t
  :mode ("\\.scala\\'" . scala-mode)
  :config
  (use-package sbt-mode
    :ensure t))

(provide 'lang-scala)
