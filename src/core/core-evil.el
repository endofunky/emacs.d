(require 'core-lib)

(use-package evil
  :ensure t
  :demand t
  :general
  (:states 'normal :prefix ef-prefix
           ":" '(eval-expression :which-key "Eval Expression")
	   "<return>" '(ef-toggle-window-fullscreen :which-key "Toggle Frame Fullscreen")
           "i" '(imenu :which-key "Open imenu")
           "U" '(universal-argument :which-key "Universal Argument")

           ;; Buffer
           "b" '(nil :which-key "Buffer")
	   "bb" '(bury-buffer :which-key "Bury Buffer")
           "bc" '(clone-indirect-buffer :which-key "Clone Buuffer")
           "bC" '(clone-indirect-buffer-other-window :which-key "Clone Buffer Other Window")
	   "bi" '(ibuffer :which-key "Open ibuffer")
           "bk" '(nil :which-key "Kill")
	   "bka" '(ef-kill-all-other-buffer :which-key "Kill All Other Buffers")
	   "bkk" '(kill-current-buffer :which-key "Kill Buffer")
	   "bkK" '(kill-buffer-and-window :which-key "Kill Buffer & Window")
	   "bko" '(ef-kill-other-buffers :which-key "Kill Other File Buffers")
	   "bks" '(kill-some-buffers :which-key "Kill Some Buffers")
	   "bl" '(evil-switch-to-windows-last-buffer :which-key "Last Buffer")
	   "bn" '(next-buffer :which-key "Next Buffer")
	   "bp" '(previous-buffer :which-key "Previous Buffer")
	   "bN" '(evil-buffer-new :which-key "New Buffer")
	   "br" '(revert-buffer :which-key "Revert Buffer")

           ;; Insert
           "I" '(nil :which-key "Insert")
           "It" '(nil :which-key "Time")
           "Iti" '(ef-insert-iso-datetime :which-key "ISO Date/Time")
           "Ito" '(ef-insert-ordinal-date :which-key "Ordinal Date")
           "Itu" '(ef-insert-unix-time :which-key "UNIX Timestamp")
           "Iu" '(ef-insert-uuid :which-key "UUID")
           "IU" '(insert-char :which-key "Unicode")

           ;; Whitespace
           "w" '(nil :which-key "Whitespace")
	   "wt" '(tabify :which-key "Spaces to Tabs")
	   "ws" '(delete-trailing-whitespace :which-key "Strip Trailing Whitespace")
	   "wu" '(untabify :which-key "Tabs to Spaces")

           ;; Window
           "W" '(nil :which-key "Window")
	   "W+" '(text-scale-adjust :which-key "Increase Text Scale")
	   "W-" '(text-scale-adjust :which-key "Decrease Text Scale")
	   "W0" '(text-scale-adjust :which-key "Reset Text Scale")
	   "Wd" '(delete-window :which-key "Delete"))
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
  (evil-undo-function 'undo-tree-undo)
  (evil-redo-function 'undo-tree-redo)
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
  (undo-tree-auto-save-history t)
  (undo-tree-enable-undo-in-region t)
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-lazy-drawing nil)
  :general
  (:states 'normal :prefix ef-prefix
	   "u" '(undo-tree-visualize :which-key "Open undo-tree"))
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
  :after evil
  :ensure t
  :config
  (global-anzu-mode t))

(use-package evil-anzu
  :after (anzu evil)
  :ensure t)

(use-package evil-lion
  :after evil
  :ensure t
  :config
  (evil-lion-mode t))

(provide 'core-evil)
