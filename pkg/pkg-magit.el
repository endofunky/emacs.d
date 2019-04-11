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
  (setenv "GIT_PAGER" ""))

(use-package evil-magit
  :after (evil magit)
  :ensure t
  :config
  (evil-magit-init)
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

  (evil-define-key 'normal magit-status-mode-map "q" #'ef-magit-quit-session)
  (evil-define-key 'visual magit-status-mode-map "q" #'ef-magit-quit-session))

(provide 'pkg-magit)
