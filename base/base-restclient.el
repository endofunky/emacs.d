(require 'base-shackle)

(use-package restclient
  :mode (("\\.rest\\'" . restclient-mode)
         ("\\.restclient\\'" . restclient-mode))
  :commands (restclient-mode)
  :ensure t
  :config
  (ef-shackle '("*HTTP Response*" :align bottom :size .4 :popup t :select t)))

(use-package ob-restclient
  :after (restclient org)
  :ensure t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((restclient . t))))

(use-package company-restclient
  :ensure t
  :after (restclient company)
  :config
  (add-to-list 'company-backends 'company-restclient))

(provide 'base-restclient)
