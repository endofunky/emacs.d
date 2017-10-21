(use-package restclient
  :commands (restclient-mode)
  :ensure t
  :config
  (add-to-list 'shackle-rules '("*HTTP Respone*" :align bottom :size .4 :popup t :select t)))

(provide 'pkg-restclient)
