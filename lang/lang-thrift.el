(use-package thrift
  :ensure t
  :mode ("\\.thrift\\'" . thrift-mode)
  :config
  (setq thrift-indent-level 2))

(provide 'lang-thrift)
