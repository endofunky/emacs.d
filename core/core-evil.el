(require 'core-lib)

(defgroup ef-keybinds nil
  "Endomacs keybinds."
  :group 'faces)

(defcustom ef-prefix ","
  "Prefix leader used for endomacs key-bindings."
  :group 'ef-theme
  :type 'string)

(use-package general
  :ensure t
  :config
  (general-auto-unbind-keys))

(use-package evil
  :ensure t
  :demand t
  :general
  ("M-h" 'evil-window-left)
  ("M-j" 'evil-window-down)
  ("M-k" 'evil-window-up)
  ("M-l" 'evil-window-right)
  (:states 'normal :prefix ef-prefix
	   "+" 'text-scale-adjust
	   "-" 'text-scale-adjust
	   "0" 'text-scale-adjust
	   "<return>" 'ef-toggle-window-fullscreen
	   "=" 'text-scale-adjust
	   "kaob" 'ef-kill-all-other-buffers
	   "kb" 'kill-this-buffer
	   "kob" 'ef-kill-other-buffers
	   "ws" 'delete-trailing-whitespace)
  :custom
  (evil-auto-indent t)
  (evil-cross-lines t)
  (evil-default-cursor t)
  (evil-default-state 'normal)
  (evil-echo-state nil)
  (evil-ex-interactive-search-highlight 'selected-window)
  (evil-ex-search-case 'smart)
  (evil-ex-search-vim-style-regexp t)
  (evil-kbd-macro-suppress-motion-error t)
  (evil-magic 'very-magic)
  (evil-search-module 'evil-search)
  (evil-shift-width 2)
  (evil-undo-system 'undo-tree)
  ;; Required for evil-collection:
  (evil-want-integration t)
  (evil-want-keybinding nil)
  :init
  (declare-function evil-ex-define-cmd "evil-ex")
  :config
  (require 'evil-ex)
  (evil-mode 1)

  ;; Avoid dropping into insert mode in compilation windows
  (add-hook 'compilation-start-hook 'evil-normal-state)

  (evil-ex-define-cmd "q" 'ef-kill-buffer-or-delete-window))

(use-package eldoc
  :after evil
  :defer t
  :config
  ;; Trigger `eldoc' after changing evil states
  (eldoc-add-command 'evil-normal-state
                     'evil-insert
                     'evil-change
                     'evil-delete
                     'evil-replace))

(use-package evil-nerd-commenter
  :after evil
  :defer t
  :ensure t
  :commands (evilnc-comment-or-uncomment-lines)
  :general
  (:states '(normal visual)
	   "\\" 'evilnc-comment-or-uncomment-lines
	   "#" 'evilnc-comment-or-uncomment-lines))

(use-package evil-surround
  :after evil
  :defer 1
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package undo-tree
  :after evil
  :ensure t
  :demand t
  :custom
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-lazy-drawing nil)
  (undo-tree-auto-save-history t)
  :general
  (:states 'normal :prefix ef-prefix
	   "u" 'undo-tree-visualize)
  :config
  (global-undo-tree-mode)
  (let ((undo-dir (expand-file-name "undo" user-emacs-directory)))
    (setq undo-tree-history-directory-alist (list (cons "." undo-dir)))))

(use-package evil-collection
  :after evil
  :ensure t
  :custom
  (evil-collection-company-use-tng nil)
  :config
  (evil-collection-init))

(provide 'core-evil)