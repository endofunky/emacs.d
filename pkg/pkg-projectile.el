(use-package projectile
  :ensure t
  :custom
  (projectile-completion-system 'ido)
  (projectile-mode-line-prefix " proj")
  :config
  (defun ef-projectile-find-file ()
    (interactive)
    (if (projectile-project-p)
        (projectile-find-file)
      (ido-find-file)))

  (evil-define-key 'normal global-map ",f" 'ef-projectile-find-file)
  (evil-define-key 'normal global-map ",p" 'projectile-switch-project)

  (projectile-global-mode t))

(provide 'pkg-projectile)
