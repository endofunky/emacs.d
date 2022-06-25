(require 'core-lib)

(defvar ef-escape-hook nil
  "Hooks to be run when normal mode is forced, eg. when hitting ESC while
already in normal mode.")

(use-package evil
  :demand t
  :general
  (:states 'normal :prefix ef-prefix
   ":"        '(eval-expression :wk "Eval expression")
   "#"        '(display-line-numbers-mode :wk "Toggle line numbers")
   "<return>" '(ef-toggle-window-fullscreen :wk "Toggle fullscreen")
   "s"        '(ef-popup-switch-buffer :wk "Switch buffer")
   "U"        '(universal-argument :wk "Universal argument")

   ;; Buffer
   "b"        '(nil :wk "Buffer")
   "bb"       '(bury-buffer :wk "Bury buffer")
   "bc"       '(clone-indirect-buffer :wk "Clone buffer")
   "bC"       '(clone-indirect-buffer-other-window :wk "Clone buffer other window")
   "bi"       '(ibuffer :wk "Open ibuffer")
   "bk"       '(nil :wk "Kill")
   "bkk"      '(kill-current-buffer :wk "Kill buffer")
   "bkK"      '(kill-buffer-and-window :wk "Kill buffer & window")
   "bkm"      '(kill-matching-buffers :wk "Kill matching buffers")
   "bko"      '(ef-kill-other-buffers :wk "Kill other file buffers")
   "bkO"      '(ef-kill-all-other-buffers :wk "Kill other buffers")
   "bks"      '(kill-some-buffers :wk "Kill some buffers")
   "bl"       '(evil-switch-to-windows-last-buffer :wk "Last buffer")
   "bn"       '(next-buffer :wk "Next buffer")
   "bp"       '(previous-buffer :wk "Previous buffer")
   "bN"       '(evil-buffer-new :wk "New buffer")
   "br"       '(revert-buffer :wk "Revert buffer")
   "by"       '(ef-yank-buffer :wk "Yank buffer")

   ;; Help
   "H"        '(nil :wk "Emacs Help")
   "Ha"       '(consult-apropos :wk "Apropos")
   "Hi"       '(info :wk "Info")
   "Hm"       '(woman :wk "Man")
   "Hw"       '(where-is :wk "Where Is")
   "Hd"       '(nil :wk "Describe")
   "HdC"      '(describe-char :wk "Character")
   "Hdc"      '(describe-coding-system :wk "Coding system")
   "Hdf"      '(describe-function :wk "Function")
   "HdF"      '(describe-face :wk "Face")
   "Hdk"      '(describe-bindings :wk "Keybinds")
   "HdK"      '(describe-keymap :wk "Keymap")
   "Hdm"      '(describe-mode :wk "Mode")
   "Hdp"      '(describe-package :wk "Package")
   "Hds"      '(describe-symbol :wk "Symbol")
   "HdS"      '(describe-syntax :wk "Syntax")
   "Hdt"      '(describe-text-properties :wk "Text properties")
   "HdT"      '(describe-theme :wk "Theme")
   "Hdv"      '(describe-variable :wk "Variable")

   ;; Insert
   "I"        '(nil :wk "Insert")
   "If"       '(nil :wk "File")
   "Iff"      '(ef-insert-file-name :wk "File name")
   "Ifb"      '(ef-insert-file-name-base :wk "File base-name")
   "Ifd"      '(ef-insert-file-name-directory :wk "File directory")
   "It"       '(nil :wk "time")
   "Iti"      '(ef-insert-iso-datetime :wk "ISO date/time")
   "Ito"      '(ef-insert-ordinal-date :wk "Ordinal date")
   "Itu"      '(ef-insert-unix-time :wk "UNIX timestamp")
   "Iu"       '(ef-insert-uuid :wk "UUID")
   "IU"       '(insert-char :wk "Unicode")

   ;; Whitespace
   "w"        '(nil :wk "Whitespace")
   "wt"       '(tabify :wk "Spaces to tabs")
   "ws"       '(delete-trailing-whitespace :wk "Strip trailing whitespace")
   "wu"       '(untabify :wk "Tabs to spaces")

   ;; Window
   "W"        '(nil :wk "Window")
   "W+"       '(text-scale-adjust :wk "Increase text scale")
   "W-"       '(text-scale-adjust :wk "Decrease text scale")
   "W0"       '(text-scale-adjust :wk "Reset text scale")
   "Wd"       '(delete-window :wk "Delete window")
   "WD"       '(delete-other-windows-internal :wk "Delete other window"))
  ;; Let emacs look up RET key behaviour in appropriate keymaps.
  (:keymaps 'evil-motion-state-map
   "RET" nil)
  :commands (evil-force-normal-state)
  :functions (ef-run-escape-hooks)
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
  (evil-want-C-w-in-emacs-state t)
  ;; When splitting the window vertically, focus the right window.
  (evil-vsplit-window-right t)
  ;; When splitting horizontally, place the new and focus window below.
  (evil-split-window-below t)
  ;; Required for evil-collection:
  (evil-want-integration t)
  (evil-want-keybinding nil)
  :commands (evil-mode)
  :config
  (require 'evil-ex)
  (declare-function evil-ex-define-cmd "evil-ex")
  (evil-mode 1)
  (evil-ex-define-cmd "q" 'ef-kill-buffer-or-delete-window)

  (defun ef-run-escape-hooks ()
    "Run ef-escape-hook hooks"
    (run-hooks 'ef-escape-hook))

  (advice-add #'evil-force-normal-state :after #'ef-run-escape-hooks)

  (defun ef-yank-buffer ()
    "Yanks all buffer contents."
    (interactive)
    (clipboard-kill-ring-save (point-min) (point-max))))

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
  :commands (evilnc-comment-or-uncomment-lines)
  :general
  (:states '(normal visual) :keymaps 'prog-mode-map
   "\\" 'evilnc-comment-or-uncomment-lines
   "#" 'evilnc-comment-or-uncomment-lines))

(use-package evil-surround
  :after evil
  :demand t
  :defer 1
  :commands (global-evil-surround-mode)
  :config
  (global-evil-surround-mode 1))

(use-package undo-tree
  :after evil
  :demand t
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-enable-undo-in-region t)
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-lazy-drawing nil)
  :commands (global-undo-tree-mode)
  :general
  (:states 'normal :prefix ef-prefix
   "u" '(undo-tree-visualize :wk "Open undo-tree"))
  :config
  (ef-add-hook undo-tree-visualizer-mode-hook
    (setq-local show-trailing-whitespace nil))

  (global-undo-tree-mode t)

  (defun ef-undo-tree-save-history-ad (orig-fn &rest args)
    "Advice for `undo-tree-save-history' to hide echo area messages."
    (let ((message-log-max nil)
          (inhibit-message t))
      (apply orig-fn args)))

  (advice-add 'undo-tree-save-history :around 'ef-undo-tree-save-history-ad))

(use-package evil-collection
  :after evil
  :demand t
  :commands (evil-collection-init)
  :config
  (evil-collection-init))

(use-package anzu
  :after evil
  :demand t
  :commands (global-anzu-mode)
  :config
  (global-anzu-mode t))

(use-package evil-anzu
  :demand t
  :after (anzu evil))

(use-package evil-lion
  :demand t
  :after evil
  :commands (evil-lion-mode)
  :config
  (evil-lion-mode t))

(provide 'core-evil)
