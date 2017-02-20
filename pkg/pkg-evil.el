(use-package evil
  :ensure t
  :diminish undo-tree-mode
  :config
  (use-package undo-tree
    :ensure t
    :config
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-lazy-drawing nil)
    (setq undo-tree-auto-save-history t)
    (let ((undo-dir (expand-file-name "undo" user-emacs-directory)))
      (setq undo-tree-history-directory-alist (list (cons "." undo-dir)))))

  (evil-mode 1)

  (setq evil-auto-indent t)
  (setq evil-cross-lines t)
  (setq evil-default-cursor t)
  (setq evil-default-state 'normal)
  (setq evil-echo-state nil)
  (setq evil-ex-search-case 'smart)
  (setq evil-ex-search-vim-style-regexp t)
  (setq evil-magic 'very-magic)
  (setq evil-search-module 'evil-search)
  (setq evil-shift-width 2)

  (defun ts/kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

  (define-key evil-normal-state-map ",kob" 'ts/kill-other-buffers)

  (defun ts/kill-buffer-or-delete-window ()
    "If more than one window is open, delete the current window, otherwise kill current buffer"
    (interactive)
    (if (> (length (window-list)) 1)
        (delete-window)
      (kill-buffer)))

  (evil-ex-define-cmd "q" 'ts/kill-buffer-or-delete-window)

  (define-key evil-visual-state-map (kbd "TAB") 'indent-region)
  (define-key evil-normal-state-map (kbd "TAB") 'indent-for-tab-command)

  (define-key evil-normal-state-map ",i" 'imenu)
  (define-key evil-normal-state-map ",ws" 'delete-trailing-whitespace)

  ;; Easier window navigation
  (define-key evil-normal-state-map (kbd "s-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "s-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "s-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "s-l") 'evil-window-right)

  (define-key evil-normal-state-map (kbd ",u") 'undo-tree-visualize)

  ;; Unset some annoying keys
  (define-key evil-motion-state-map "K" nil)
  (define-key evil-normal-state-map "K" nil)

  (evil-add-hjkl-bindings package-menu-mode-map 'emacs
    "H" 'package-menu-quick-help)

  (use-package evil-cleverparens
    :ensure t
    :diminish evil-cleverparens-mode
    :commands (evil-cleverparens-mode)
    :init
    (add-hook 'emacs-lisp-mode-hook 'evil-cleverparens-mode)
    (setq evil-cleverparens-use-additional-movement-keys nil))

  (use-package evil-nerd-commenter
    :defer 1
    :ensure t
    :commands (evilnc-comment-or-uncomment-lines)
    :init
    (define-key evil-normal-state-map (kbd "\\") 'evilnc-comment-or-uncomment-lines)
    (define-key evil-visual-state-map (kbd "\\") 'evilnc-comment-or-uncomment-lines))

  (use-package evil-surround
    :defer 1
    :ensure t
    :config
    (global-evil-surround-mode 1)))

(provide 'pkg-evil)
