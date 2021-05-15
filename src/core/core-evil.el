(require 'core-lib)

(use-package evil
  :ensure t
  :demand t
  :general
  ("M-h" 'evil-window-left)
  ("M-j" 'evil-window-down)
  ("M-k" 'evil-window-up)
  ("M-l" 'evil-window-right)
  (:states 'visual :prefix ef-prefix
	   "=" 'ef-align-to-=)
  (:states 'normal :prefix ef-prefix
	   "<return>" 'ef-toggle-window-fullscreen
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
  (:states '(normal visual) :keymaps 'prog-mode-map
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
  (global-undo-tree-mode))

(use-package evil-collection
  :after evil
  :ensure t
  :custom
  (evil-collection-company-use-tng nil)
  :config
  (evil-collection-init))

(use-package anzu
  :ensure t
  :config
  (global-anzu-mode t))

(use-package evil-anzu
  :ensure t
  :after (anzu evil))

(provide 'core-evil)
