(use-package restclient
  :commands (restclient-mode)
  :ensure t
  :config
  (ts/shackle '("*HTTP Respone*" :align bottom :size .4 :popup t :select t)))

(provide 'pkg-restclient)
