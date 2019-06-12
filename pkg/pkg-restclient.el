(require 'pkg-shackle)

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

(use-package company-restclient
  :ensure t
  :after restclient
  :config
  (add-to-list 'company-backend 'company-restclient)
  (add-hook 'restclient-mode-hook 'company-mode-on))

(provide 'pkg-restclient)
