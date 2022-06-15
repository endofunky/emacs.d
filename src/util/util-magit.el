(require 'core-evil)
(require 'core-shackle)

(use-package magit
  :defer 2
  :commands (magit-blame
             magit-checkout
             magit-diff
             magit-log-all
             magit-log
             magit-status)
  :custom
  (magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
  (magit-display-buffer-function #'ef-magit-display-buffer-function)
  (magit-bury-buffer-function #'magit-restore-window-configuration)
  :functions (magit-get-current-branch)
  :init
  :general
  (:prefix ef-prefix :states 'normal
   "g" '(nil :wk "Magit")
   "gd" '(magit-diff :wk "Diff")
   "gs" '(magit-status :wk "Status")
   "gl" '(magit-log :wk "Log")
   "gL" '(magit-log-all :wk "Log (All)")
   "gb" '(magit-blame :wk "Blame")
   "gc" '(magit-checkout :wk "Checkout"))
  (:prefix ef-prefix :states '(visual)
   "g" '(nil :wk "Magit")
   "gl" '(magit-log :wk "Log"))
  :config
  (ef-shackle '(magit-diff-mode :align right :size .5 :popup t :select nil :float t))
  (ef-shackle '(magit-process-mode :align below :size .2 :popup t :select nil :float t))

  ;; This depends on the two shackle rules above to work correctly
  (defun ef-magit-display-buffer-function (buffer)
    "Open magit windows full-frame, except for `magit-process-mode' buffers and
windows opened from `git-commit-mode'."
    (if (or (bound-and-true-p git-commit-mode)
            (eq (buffer-local-value 'major-mode buffer) 'magit-process-mode))
        (display-buffer buffer)
      (display-buffer-full-frame buffer '(magit--display-buffer-fullframe))))

  (ef-add-hook git-commit-setup-hook :fn ef-git-commit-jira-ticket-hook
    "Add JIRA tickets as prefix to commit message if the branch name starts with
what looks like a JIRA ticket ID."
    (if-let* ((branch (magit-get-current-branch))
              (segments (split-string branch "[\\/\\.-]"))
              (_ (string-match "^[A-Z]+-[0-9]+" branch))
              (label (format "[%s-%s] " (car segments) (cadr segments))))
        (unless (string-match (regexp-quote label) (buffer-string))
          (insert label)))
    (end-of-line))

  (transient-append-suffix 'magit-pull "C"
    '("A" "Autostash" "--autostash"))

  (setenv "GIT_PAGER" ""))

(use-package magit-todos
  :after magit
  :functions (magit-todos-mode)
  :config
  (require 'org)
  (magit-todos-mode t))

(provide 'util-magit)
