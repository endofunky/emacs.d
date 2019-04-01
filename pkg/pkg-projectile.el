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

  (projectile-global-mode t)

  ;; Workaround for slow down caused by modeline updates of projectile
  ;; See: https://github.com/bbatsov/projectile/issues/1183
  (add-to-list 'projectile-globally-ignored-directories "elpa")
  (add-to-list 'projectile-globally-ignored-directories ".cache")
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories ".bundle")
  (add-to-list 'projectile-globally-ignored-directories ".git"))

(provide 'pkg-projectile)
