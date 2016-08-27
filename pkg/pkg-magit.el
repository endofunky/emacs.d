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

(use-package diff-hl
  :if window-system
  :ensure t
  :commands (diff-hl-mode diff-hl-dir-mode)
  :init
  (add-hook 'prog-mode-hook 'diff-hl-mode)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'org-mode-hook 'diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :config
  (require 'diff-hl-dired)
  (require 'diff-hl-flydiff)

  (setq diff-hl-side 'right)
  (setq diff-hl-draw-borders t)

  (use-package fringe-helper
    :ensure t
    :config
    (fringe-helper-define 'ts/diff-hl-added    '(top repeat) "XXXXXXXX")
    (fringe-helper-define 'ts/diff-hl-modified '(top repeat) "XXXXXXXX")
    (fringe-helper-define 'ts/diff-hl-deleted  '(top repeat) "XXXXXXXX"))

  (defun ts/diff-hl-fringe-bmp-from-type (type _pos)
    (cl-case type
      (unknown 'question-mark)
      (change 'ts/diff-hl-modified)
      (insert 'ts/diff-hl-added)
      (delete 'ts/diff-hl-deleted)
      (ignored 'diff-hl-bmp-i)
      (t (intern (format "diff-hl-bmp-%s" type)))))

  (diff-hl-flydiff-mode t)

  (setq diff-hl-fringe-bmp-function 'ts/diff-hl-fringe-bmp-from-type)

  ;; Workaround for flydiff breaking company-mode keybinds
  (defun ts/disable-hl-flydiff-mode (&rest ignore)
    (diff-hl-flydiff-mode -1))

  (defun ts/enable-hl-flydiff-mode (&rest ignore)
    (diff-hl-flydiff-mode t))

  (add-hook 'company-completion-started-hook 'ts/disable-hl-flydiff-mode)
  (add-hook 'company-completion-finished-hook 'ts/enable-hl-flydiff-mode)
  (add-hook 'company-completion-cancelled-hook 'ts/enable-hl-flydiff-mode))

(provide 'pkg-magit)
