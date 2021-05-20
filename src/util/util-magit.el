(use-package magit
  :ensure t
  :commands (magit-blame
             magit-checkout
             magit-diff
             magit-log-all
             magit-log
             magit-status)
  :custom
  (magit-completing-read-function 'ivy-completing-read)
  (magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
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
  :config
  (ef-add-hook git-commit-setup-hook :fn ef-git-commit-jira-ticket-hook
    (if-let* ((branch (magit-get-current-branch))
              (segments (split-string branch "[\\/\\.-]"))
              (_ (string-match "^[A-Z]+-[0-9]+" branch))
              (label (format "[%s-%s] " (car segments) (cadr segments))))
        (unless (string-match (regexp-quote label) (buffer-string))
          (insert label)))
    (end-of-line))

  (ef-shackle '(magit-diff-mode :align right :size .5 :popup t :select nil :float t))

  (transient-append-suffix 'magit-pull "C"
    '("A" "Autostash" "--autostash"))

  ;; Don't let magit-status mess up window configurations
  ;; http://whattheemacsd.com/setup-magit.el-01.html
  (defadvice magit-status (around magit-fullscreen activate)
    (when (fboundp 'ef-flycheck-close-window)
      (ef-flycheck-close-window))
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun ef-magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))

  (evil-define-key 'normal magit-status-mode-map "q" #'ef-magit-quit-session)
  (evil-define-key 'visual magit-status-mode-map "q" #'ef-magit-quit-session)

  (setenv "GIT_PAGER" ""))

(use-package magit-todos
  :after magit
  :ensure t
  :general
  (:states 'normal :prefix ef-prefix
           "pT" '(ivy-magit-todos :wk "Search TODOs"))
  :config
  (require 'org)
  (magit-todos-mode t))

(provide 'util-magit)
