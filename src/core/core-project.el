(require 'core-evil)

(use-package project
  :straight nil
  :commands (ef-project-find-file)
  :general
  ([remap find-file] 'ef-project-find-file)
  (:states 'normal :prefix ef-prefix
   "f"  '(ef-project-find-file :wk "Find File")
   "p"  '(nil :wk "Project")
   "p!" '(project-shell-command :wk "Run Shell Command in Project Root")
   "pf" '(project-find-file :wk "Find File in Project")
   "pF" '(project-forget-project :wk "Forget Project")
   "pS" '(ef-project-shell :wk "Open Project Shell")
   "pd" '(project-find-dir :wk "Open Dired in Project Directory")
   "pD" '(project-dired :wk "Open Dired in Project Root")
   "pv" '(project-vc-dir :wk "Run Vc-Dir in Project Root")
   "pk" '(project-kill-buffers :wk "Kill Project Buffers")
   "pp" '(ef-project-switch-project :wk "Switch Project")
   "ps" '(project-switch-to-buffer :wk "Switch to Project Buffer"))
  :config
  (defun ef-project-shell ()
    "Open VTerm Shell in Project Root"
    (interactive)
    (let ((root (project-root (project-current t))))
      (let ((default-directory root)
            (project-current-inhibit-prompt t))
        (vterm))))

  (defun ef-project-switch-project (dir)
    "\"Switch\" to another project and find file."
    (interactive (list (project-prompt-project-dir)))
    (let ((default-directory dir)
          (project-current-inhibit-prompt t))
      (call-interactively #'project-find-file)))

  (defun ef-project-root ()
    "Return the current project root or nil if not in a project."
    (when-let ((project (project-current nil)))
      (project-root project)))

  (defun ef-project-find-file ()
    "If in a project call `project-find-file', otherwise call `find-file'."
    (interactive)
    (if (project-current)
        (project-find-file)
      (call-interactively #'find-file))))

(provide 'core-project)
