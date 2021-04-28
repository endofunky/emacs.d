(use-package projectile
  :ensure t
  :custom
  (projectile-completion-system 'ivy)
  (projectile-mode-line-prefix " P")
  :config
  (defun ef-projectile-find-file ()
    (interactive)
    (if (projectile-project-p)
        (projectile-find-file)
      (counsel-file-jump)))

  (evil-define-key 'normal global-map ",f" 'ef-projectile-find-file)
  (evil-define-key 'normal global-map ",p" 'projectile-switch-project)
  (evil-define-key 'normal projectile-mode-map ",l" 'projectile-toggle-between-implementation-and-test)

  (projectile-global-mode t))

(provide 'base-projectile)
