(use-package magit
  :ensure t
  :commands (magit-status
             magit-log-popup
             magit-log-all
             magit-blame-mode
             magit-diff-popup
             magit-diff)
  :diminish auto-revert-mode
  :init
  (defun ts/magit-diff-head ()
    "Execute `magit-diff' against current HEAD."
    (interactive)
    (magit-diff "HEAD"))

  (define-key evil-normal-state-map ",gd" 'magit-diff-popup)
  (define-key evil-normal-state-map ",gD" 'ts/magit-diff-head)
  (define-key evil-normal-state-map ",gs" 'magit-status)
  (define-key evil-normal-state-map ",gl" 'magit-log-popup)
  (define-key evil-normal-state-map ",gL" 'magit-log-all)
  (define-key evil-normal-state-map ",gb" 'magit-blame-mode)
  :config
  (use-package evil-magit
    :ensure t
    :config
    (evil-magit-init))

  (setenv "GIT_PAGER" "")
  (setq magit-commit-show-diff nil)
  (setq magit-stage-all-confirm nil)
  (setq magit-stage-all-confirm nil)
  (setq magit-log-arguments '("--graph"
                              "--decorate"
                              "--color"
                              "--show-signature"))

  ;; Don't let magit-status mess up window configurations
  ;; http://whattheemacsd.com/setup-magit.el-01.html
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun ts/magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))

  (define-key magit-status-mode-map (kbd "q") 'ts/magit-quit-session))

(use-package gitconfig-mode
  :ensure t
  :mode ("/\\.gitconfig\\'" "/\\.git/config\\'" "/git/config\\'" "/\\.gitmodules\\'"))

(use-package gitignore-mode
  :ensure t
  :mode ("/\\.gitignore\\'" "/\\.git/info/exclude\\'" "/git/ignore\\'"))

(use-package git-gutter-fringe
  :ensure t
  :diminish ""
  :init
  :config
  (setq git-gutter:update-interval 2
        git-gutter:diff-option "-w"
        git-gutter:hide-gutter nil
        git-gutter:ask-p nil
        git-gutter:verbosity 0
        git-gutter:separator-sign " "
        git-gutter:unchanged-sign " "
        git-gutter:handled-backends '(git hg bzr svn)
        git-gutter-fr:side 'right-fringe)

  (fringe-helper-define 'git-gutter-fr:added nil
    "..XX.."
    "..XX.."
    "XXXXXX"
    "XXXXXX"
    "..XX.."
    "..XX..")

  (fringe-helper-define 'git-gutter-fr:deleted nil
    "......"
    "......"
    "XXXXXX"
    "XXXXXX"
    "......"
    "......")

  (fringe-helper-define 'git-gutter-fr:modified nil
    "......"
    ".XXXX."
    ".XXXX."
    ".XXXX."
    ".XXXX."
    "......")

  (global-git-gutter-mode))

(provide 'pkg-magit)
