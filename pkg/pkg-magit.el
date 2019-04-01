(use-package magit
  :ensure t
  :commands (magit-blame
             magit-checkout
             magit-diff
             magit-log-all
             magit-log
             magit-status)
  :diminish auto-revert-mode
  :custom
  (magit-completing-read-function 'magit-ido-completing-read)
  :init
  (defun ef-magit-diff-head ()
    "Execute `magit-diff' against current HEAD."
    (interactive)
    (magit-diff "HEAD"))

  (define-key evil-normal-state-map ",gd" 'magit-diff)
  (define-key evil-normal-state-map ",gD" 'ef-magit-diff-head)
  (define-key evil-normal-state-map ",gs" 'magit-status)
  (define-key evil-normal-state-map ",gl" 'magit-log)
  (define-key evil-normal-state-map ",gL" 'magit-log-all)
  (define-key evil-normal-state-map ",gb" 'magit-blame)
  (define-key evil-normal-state-map ",gc" 'magit-checkout)
  :config
  (setenv "GIT_PAGER" "")

  ;; Don't let magit-status mess up window configurations
  ;; http://whattheemacsd.com/setup-magit.el-01.html
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    (if (fboundp 'shackle-mode)
        (shackle-mode -1))
    ad-do-it
    (delete-other-windows))

  (defun ef-magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen)
    (if (fboundp 'shackle-mode)
        (shackle-mode t)))

  (define-key magit-status-mode-map (kbd "q") 'ef-magit-quit-session)

  (defun ef-magit-wip-all ()
    "Create a wip commit with all changes on HEAD"
    (interactive)
    (magit-stage-modified t)
    (magit-commit '("-m" "wip [ci skip]")))

  (define-key magit-status-mode-map (kbd ", w") 'ef-magit-wip-all)

  (defun ef-magit-undo-commit ()
    "Undo the HEAD~1 commit"
    (interactive)
    (when (yes-or-no-p "Reset the last commit?")
      (magit-reset "HEAD~1")))

  (define-key magit-status-mode-map (kbd ", u") 'ef-magit-undo-commit))

(use-package evil-magit
  :after magit
  :ensure t
  :config
  (evil-magit-init))

(provide 'pkg-magit)
