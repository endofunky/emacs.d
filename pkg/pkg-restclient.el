(use-package restclient
  :mode "\\.rest$"
  :commands (restclient-mode)
  :ensure t
  :config
  (ef-shackle '("*HTTP Response*" :align bottom :size .4 :popup t :select t)))

(provide 'pkg-restclient)
