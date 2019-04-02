(use-package rg
  :ensure t
  :commands (rg rg-project)
  :init
  (defun ef-rg ()
    "Run rg-project if in a project, otherwise rg"
    (interactive)
    (call-interactively
     (if (projectile-project-p)
         #'rg-project
       #'rg)))

  (define-key evil-normal-state-map ",/" #'ef-rg))

(provide 'pkg-rg)
