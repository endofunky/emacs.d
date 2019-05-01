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
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

  (define-key evil-normal-state-map ",kob" 'ef-kill-other-buffers)
  (define-key evil-normal-state-map ",kb" 'kill-this-buffer)

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
  (define-key evil-normal-state-map "K" nil)

  (defvar ef-toggle-scratch--prev-buffer nil)

  (defun ef-toggle-scratch--goto-scratch ()
    (if-let* ((scratch-buffer (get-buffer "*scratch*")))
        (progn
          (setq ef-toggle-scratch--prev-buffer (current-buffer))
          (switch-to-buffer scratch-buffer))
      (message "No *scratch* buffer found.")))

  (defun ef-toggle-scratch--goto-prev-buffer ()
    (if (buffer-live-p ef-toggle-scratch--prev-buffer)
        (switch-to-buffer ef-toggle-scratch--prev-buffer)
      (message "No buffer to switch back to.")))

  (defun ef-toggle-scratch ()
    "Toggle between *scratch* buffer and the current buffer."
    (interactive)
    (if (equal (buffer-name) "*scratch*")
        (ef-toggle-scratch--goto-prev-buffer)
      (ef-toggle-scratch--goto-scratch)))

  (define-key evil-normal-state-map ",S" 'ef-toggle-scratch))

(use-package evil-cleverparens
  :after evil
  :ensure t
  :diminish evil-cleverparens-mode
  :commands (evil-cleverparens-mode
             evil-cp-wrap-previous-round
             evil-cp-wrap-next-round
             evil-cp-wrap-previous-square
             evil-cp-wrap-next-square
             evil-cp-wrap-previous-curly
             evil-cp-wrap-next-curly)
  :custom
  (evil-cleverparens-use-additional-movement-keys nil)
  :init
  (add-hook 'emacs-lisp-mode-hook 'evil-cleverparens-mode)
  (evil-define-key 'normal prog-mode-map (kbd "M-(") 'evil-cp-wrap-next-round)
  (evil-define-key 'normal prog-mode-map (kbd "M-)") 'evil-cp-wrap-previous-round)
  (evil-define-key 'normal prog-mode-map (kbd "M-[") 'evil-cp-wrap-next-square)
  (evil-define-key 'normal prog-mode-map (kbd "M-]") 'evil-cp-wrap-previous-square)
  (evil-define-key 'normal prog-mode-map (kbd "M-{") 'evil-cp-wrap-next-curly)
  (evil-define-key 'normal prog-mode-map (kbd "M-}") 'evil-cp-wrap-previous-curly))

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
  :diminish undo-tree-mode
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
