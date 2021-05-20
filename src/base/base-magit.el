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
           "g" '(nil :which-key "Magit")
           "gd" '(magit-diff :which-key "Diff")
           "gs" '(magit-status :which-key "Status")
           "gl" '(magit-log :which-key "Log")
           "gL" '(magit-log-all :which-key "Log (All)")
           "gb" '(magit-blame :which-key "Blame")
           "gc" '(magit-checkout :which-key "Checkout"))
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

(use-package transient
  :defer t
  :ensure t
  :config
  (transient-bind-q-to-quit)

  (defadvice transient-setup (before transient-setup activate)
    (ef-transient-suspend-shackle-mode))

  (defun ef-transient-suspend-shackle-mode ()
    (when (bound-and-true-p shackle-mode)
      (shackle-mode -1)
      (add-hook 'transient-exit-hook 'ef-transient-resume-shackle-mode)))

  (defun ef-transient-resume-shackle-mode ()
    (unless transient--prefix
      (shackle-mode t)
      (remove-hook 'transient-exit-hook 'ef-transient-resume-shackle-mode))))

(provide 'base-magit)
