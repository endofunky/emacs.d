(require 'core-evil)

(use-package projectile
  :ensure t
  :custom
  (projectile-completion-system 'ivy)
  (projectile-mode-line-prefix " P")
  :general
  (:states 'normal :prefix ef-prefix
	   "f" 'ef-projectile-find-file
	   "p" 'projectile-switch-project)
  (:states 'normal :keymaps 'projectile-mode-map :prefix ef-prefix
	   "l" 'projectile-toggle-between-implementation-and-test)
  :config
  (declare-function projectile-project-p "projectile")
  (defun ef-projectile-find-file ()
    (interactive)
    (if (projectile-project-p)
        (projectile-find-file)
      (counsel-file-jump)))

  (projectile-mode t))

(provide 'core-projectile)
