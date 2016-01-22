(use-package scala-mode2
  :ensure t
  :mode ("\\.scala\\'" . scala-mode)
  :init
  (use-package sbt-mode
    :ensure t))

(provide 'lang-scala)
