(use-package magit
  :ensure t
  :commands (magit-blame-popup
             magit-checkout
             magit-diff
             magit-diff-popup
             magit-log-all
             magit-log-popup
             magit-status)
  :diminish auto-revert-mode
  :init
  (defun ef-magit-diff-head ()
    "Execute `magit-diff' against current HEAD."
    (interactive)
    (magit-diff "HEAD"))

  (define-key evil-normal-state-map ",gd" 'magit-diff-popup)
  (define-key evil-normal-state-map ",gD" 'ef-magit-diff-head)
  (define-key evil-normal-state-map ",gs" 'magit-status)
  (define-key evil-normal-state-map ",gl" 'magit-log-popup)
  (define-key evil-normal-state-map ",gL" 'magit-log-all)
  (define-key evil-normal-state-map ",gb" 'magit-blame-popup)
  (define-key evil-normal-state-map ",gc" 'magit-checkout)
  (define-key evil-normal-state-map ",gc" 'magit-checkout)
  :config
  (setenv "GIT_PAGER" "")
  (setq magit-commit-show-diff nil)
  (setq magit-stage-all-confirm nil)
  (setq magit-stage-all-confirm nil)
  (setq magit-completing-read-function 'magit-ido-completing-read)

  ;; Don't let magit-status mess up window configurations
  ;; http://whattheemacsd.com/setup-magit.el-01.html
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun ef-magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))

  (define-key magit-status-mode-map (kbd "q") 'ef-magit-quit-session))

(use-package evil-magit
  :after magit
  :ensure t
  :config
  (evil-magit-init))

(use-package gitattributes-mode
  :ensure t)

(use-package gitconfig-mode
  :ensure t
  :mode ("/\\.gitconfig\\'" "/\\.git/config\\'" "/git/config\\'" "/\\.gitmodules\\'"))

(use-package gitignore-mode
  :ensure t
  :mode ("/\\.gitignore\\'" "/\\.git/info/exclude\\'" "/git/ignore\\'"))

(provide 'pkg-magit)
