;;; lang-magit.el --- Magit/Git configuration -*- lexical-binding: t; -*-
(require 'core-evil)
(require 'core-popup)

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
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (magit-bury-buffer-function #'magit-restore-window-configuration)
  :functions (magit-get-current-branch)
  :general
  (:prefix ef-leader :states 'normal
   "g" '(nil :wk "Git")
   "gd" '(magit-diff :wk "Diff")
   "gi" '(magit-init :wk "Init")
   "gs" '(magit-status :wk "Status")
   "gl" '(magit-log :wk "Log")
   "gL" '(magit-log-all :wk "Log (all)")
   "gb" '(magit-blame :wk "Blame")
   "gc" '(magit-checkout :wk "Checkout"))
  (:prefix ef-leader :states 'visual
   "g" '(nil :wk "Git")
   "gl" '(magit-log :wk "Log"))
  :config
  (+add-hook git-commit-setup-hook :fn +git-commit-add-jira-ticket-h
    "Add JIRA tickets as prefix to commit message if the branch name starts with
what looks like a JIRA ticket ID."
    (if-let* ((branch (magit-get-current-branch))
              (segments (split-string branch "[\\/\\.-]"))
              (ok (string-match "^[A-Z]+-[0-9]+" branch))
              (label (format "[%s-%s] "
                             (upcase (car segments))
                             (cadr segments))))
        (unless (string-match (regexp-quote label) (buffer-string))
          (insert label)))
    (end-of-line))

  (transient-append-suffix 'magit-pull "C"
    '("A" "Autostash" "--autostash"))

  (setenv "GIT_PAGER" ""))

(use-package git-modes)

(use-package magit-todos
  :after magit-status
  :functions (magit-todos-mode)
  :config
  ;; https://github.com/alphapapa/magit-todos/issues/24
  (custom-set-variables
   '(magit-todos-keywords (list "HOLD"
                                "TODO"
                                "NEXT"
                                "THEM"
                                "PROG"
                                "OKAY"
                                "DONT"
                                "FAIL"
                                "MAYBE"
                                "KLUDGE"
                                "HACK"
                                "TEMP"
                                "FIXME"
                                "XXX"
                                "XXXX")))
    (magit-todos-mode t))

(provide 'util-magit)
