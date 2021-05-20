(require 'core-evil)

(use-package projectile
  :ensure t
  :custom
  (projectile-completion-system 'ivy)
  (projectile-mode-line-prefix " P")
  (projectile-cache-file
   (no-littering-expand-var-file-name "projectile/cache.el"))
  (projectile-known-projects-file
   (no-littering-expand-var-file-name "projectile/known-projects.el"))
  (projectile-mode-line-function 'ef-projectile-mode-line)
  :commands (projectile-toggle-between-implementation-and-test)
  :general
  (:states 'normal :prefix ef-prefix
	   "f" '(ef-projectile-find-file :which-key "Find File in Project")
	   "p" '(projectile-switch-project :which-key "Switch Project"))
  :config
  (declare-function projectile-project-p "projectile")
  (declare-function projectile-project-name "projectile")

  (defun ef-projectile-mode-line ()
    (format "%s[%s]"
            projectile-mode-line-prefix
            (projectile-project-name)))

  (defun ef-projectile-find-file ()
    (interactive)
    (if (projectile-project-p)
        (projectile-find-file)
      (counsel-file-jump)))

  (projectile-mode t))

(provide 'core-projectile)
