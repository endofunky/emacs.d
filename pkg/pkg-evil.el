(use-package evil
  :ensure t
  :custom
  (evil-auto-indent t)
  (evil-cross-lines t)
  (evil-default-cursor t)
  (evil-default-state 'normal)
  (evil-echo-state nil)
  (evil-ex-search-case 'smart)
  (evil-ex-search-vim-style-regexp t)
  (evil-magic 'very-magic)
  (evil-search-module 'evil-search)
  (evil-shift-width 2)
  ;; Required for evil-collection:
  (evil-want-integration t)
  (evil-want-keybinding nil)
  :config
  (evil-mode 1)

  ;; Avoid dropping into insert mode in compilation windows
  (add-hook 'compilation-start-hook 'evil-normal-state)

  (defun ef-kill-other-buffers ()
    "Kill all other buffers except for special buffers."
    (interactive)
    (dolist (buf (delq (current-buffer) (buffer-list)))
      (unless (or (eq 'exwm-mode (with-current-buffer buf
                                   major-mode))
                  (string-prefix-p "*" (string-trim (buffer-name buf))))
        (if-let ((win (get-buffer-window buf)))
            (delete-window win))
        (kill-buffer buf))))

  (defun ef-kill-all-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (dolist (buf (delq (current-buffer) (buffer-list)))
      (if-let ((win (get-buffer-window buf)))
          (delete-window win))
      (kill-buffer buf)))

  (define-key evil-normal-state-map ",kob" 'ef-kill-other-buffers)
  (define-key evil-normal-state-map ",kaob" 'ef-kill-all-other-buffers)
  (define-key evil-normal-state-map ",kb" 'kill-this-buffer)

  (global-set-key (kbd "M-h") #'evil-window-left)
  (global-set-key (kbd "M-j") #'evil-window-down)
  (global-set-key (kbd "M-k") #'evil-window-up)
  (global-set-key (kbd "M-l") #'evil-window-right)

  (defun ef-kill-buffer-or-delete-window ()
    "If more than one window is open, delete the current window, otherwise kill current buffer"
    (interactive)
    (if (> (length (window-list)) 1)
        (delete-window)
      (kill-buffer)))

  (evil-ex-define-cmd "q" 'ef-kill-buffer-or-delete-window)

  (defun ef-indent-buffer ()
    "Indent the currently visited buffer."
    (interactive)
    (indent-region (point-min) (point-max)))

  (define-key evil-normal-state-map (kbd ", TAB") 'ef-indent-buffer)

  (define-key evil-visual-state-map (kbd "TAB") 'indent-region)
  (define-key evil-normal-state-map (kbd "TAB") 'indent-for-tab-command)

  (define-key evil-normal-state-map ",i" 'imenu)
  (define-key evil-normal-state-map ",d" 'dired)
  (define-key evil-normal-state-map ",ws" 'delete-trailing-whitespace)

  ;; Alignment
  (defun ef-align-to-= (begin end)
    "Align region to = signs"
    (interactive "r")
    (align-regexp begin end "\\(\\s-*\\)=" 1 1 ))

  (evil-define-key 'visual prog-mode-map ",=" 'ef-align-to-=)

  ;; Easier window navigation
  (define-key evil-normal-state-map (kbd "s-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "s-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "s-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "s-l") 'evil-window-right)

  ;; Text-scaling
  (define-key evil-normal-state-map ",-" 'text-scale-adjust)
  (define-key evil-normal-state-map ",+" 'text-scale-adjust)
  (define-key evil-normal-state-map ",=" 'text-scale-adjust)

  ;; Comint history
  (evil-define-key 'insert comint-mode-map (kbd "<up>") 'comint-previous-input)
  (evil-define-key 'insert comint-mode-map (kbd "<down>") 'comint-next-input)

  ;; Xref
  (evil-define-key 'normal prog-mode-map ",." 'pop-tag-mark)

  ;; Unset some annoying keys
  (define-key evil-motion-state-map "K" nil)
  (define-key evil-normal-state-map "K" nil))

(use-package evil-nerd-commenter
  :after evil
  :defer t
  :ensure t
  :commands (evilnc-comment-or-uncomment-lines)
  :init
  (define-key evil-normal-state-map (kbd "\\") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-visual-state-map (kbd "\\") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-normal-state-map (kbd "#") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-visual-state-map (kbd "#") 'evilnc-comment-or-uncomment-lines))

(use-package evil-surround
  :after evil
  :defer 1
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package undo-tree
  :after evil
  :ensure t
  :custom
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-lazy-drawing nil)
  (undo-tree-auto-save-history t)
  :config
  (global-undo-tree-mode)
  (let ((undo-dir (expand-file-name "undo" user-emacs-directory)))
    (setq undo-tree-history-directory-alist (list (cons "." undo-dir))))

  (define-key evil-normal-state-map (kbd ",u") 'undo-tree-visualize))

(use-package evil-collection
  :after evil
  :ensure t
  :custom
  (evil-collection-company-use-tng nil)
  :config
  (evil-collection-init)
  ;; Overrides
  (evil-define-key '(normal visual) dired-mode-map "q" #'kill-this-buffer))

(provide 'pkg-evil)
