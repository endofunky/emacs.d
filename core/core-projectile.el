(require 'core-evil)

(use-package projectile
  :ensure t
  :custom
  (projectile-completion-system 'ivy)
  (projectile-mode-line-prefix " P")
  :commands (projectile-toggle-between-implementation-and-test)
  :general
  (:states 'normal :prefix ef-prefix
	   "f" 'ef-projectile-find-file
	   "p" 'projectile-switch-project)
  :config
  (declare-function projectile-project-p "projectile")
  (defun ef-projectile-find-file ()
    (interactive)
    (if (projectile-project-p)
        (projectile-find-file)
      (counsel-file-jump)))

  (projectile-mode t))

(provide 'core-projectile)
