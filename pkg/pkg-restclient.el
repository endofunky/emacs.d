(use-package restclient
  :mode "\\.rest$"
  :commands (restclient-mode)
  :ensure t
  :config
  (ef-shackle '("*HTTP Response*" :align bottom :size .4 :popup t :select t)))

(use-package ob-restclient
  :after org
  :ensure t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((restclient . t))))

(provide 'pkg-restclient)
