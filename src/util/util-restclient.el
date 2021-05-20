(require 'core-shackle)

(use-package restclient
  :mode (("\\.rest\\'" . restclient-mode)
         ("\\.restclient\\'" . restclient-mode))
  :commands (restclient-mode)
  :ensure t
  :config
  (ef-add-popup "*HTTP Response*"))

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

(provide 'util-restclient)
