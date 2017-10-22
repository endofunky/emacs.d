(use-package projectile
  :ensure t
  :defer 2
  :commands (projectile-switch-project projectile-find-file projectile-ag)
  :init
  (evil-define-key 'normal global-map ",f" 'projectile-find-file)
  (evil-define-key 'normal global-map ",p" 'projectile-switch-project)
  (evil-define-key 'normal global-map ",/" 'projectile-ag)
  :config
  (projectile-global-mode t)
  (setq projectile-completion-system 'ido)
  ;; Workaround for slow down caused by modeline updates of projectile
  ;; See: https://github.com/bbatsov/projectile/issues/1183
  (setq projectile-mode-line
        '(:eval (format " proj[%s]"
                        (projectile-project-name))))
  (add-to-list 'projectile-globally-ignored-directories "elpa")
  (add-to-list 'projectile-globally-ignored-directories ".cache")
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories ".bundle")
  (add-to-list 'projectile-globally-ignored-directories ".git"))

(provide 'pkg-projectile)
