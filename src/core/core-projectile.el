(require 'core-evil)

(use-package projectile
  :ensure t
  :custom
  (projectile-completion-system 'default)
  (projectile-mode-line-prefix " P")
  (projectile-cache-file
   (no-littering-expand-var-file-name "projectile/cache.el"))
  (projectile-known-projects-file
   (no-littering-expand-var-file-name "projectile/known-projects.el"))
  (projectile-mode-line-function 'ef-projectile-mode-line)
  (projectile-indexing-method 'hybrid)
  :commands (projectile-toggle-between-implementation-and-test)
  :general
  (:states 'normal :prefix ef-prefix
   "f"  '(ef-projectile-find-file :wk "Find File")
   "p"  '(nil :wk "Project")
   "p!" '(projectile-run-shell-command-in-root :wk "Run Command in Project Root")
   "pa" '(projectile-add-known-project :wk "Add Project")
   "pd" '(projectile-remove-known-project :wk "Remove Known Project")
   "pf" '(projectile-find-file :wk "Find File in Project")
   "pi" '(consult-project-imenu :wk "imenu in Project")
   "pk" '(projectile-kill-buffers :wk "Kill Project Buffers")
   "pp" '(projectile-switch-project :wk "Switch Project")
   "ps" '(projectile-switch-to-buffer :wk "Switch Project Buffer")
   "pS" '(projectile-save-project-buffers :wk "Save Project Buffers"))
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
      (call-interactively #'find-file)))

  (projectile-mode t))

(provide 'core-projectile)
