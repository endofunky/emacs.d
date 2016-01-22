(use-package projectile
  :ensure t
  :defer 2
  :commands (projectile-switch-project projectile-find-file)
  :init
  (evil-define-key 'normal global-map ",f" 'projectile-find-file)
  (evil-define-key 'normal global-map ",p" 'projectile-switch-project)
  :config
  (projectile-global-mode t)
  (setq projectile-completion-system 'ido)
  (add-to-list 'projectile-globally-ignored-directories "elpa")
  (add-to-list 'projectile-globally-ignored-directories ".cache")
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories ".bundle")
  (add-to-list 'projectile-globally-ignored-directories ".git"))

(provide 'pkg-projectile)
