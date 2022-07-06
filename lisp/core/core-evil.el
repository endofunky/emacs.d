;;; core-evil.el --- Evil-mode vim emulation -*- lexical-binding: t; -*-
(require 'core-lib)
(require 'core-popup)

(defvar ef-escape-hook nil
  "Hooks to be run when normal mode is forced, eg. when hitting ESC while
already in normal mode.")

(use-package evil
  :demand t
  :general
  (:states 'normal :prefix ef-leader
   ":"        '(eval-expression :wk "Eval expression")
   "#"        '(display-line-numbers-mode :wk "Toggle line numbers")
   "s"        '(switch-to-buffer :wk "Change buffer")
   "F"        '(+sudo-find-file :wk "Find file (sudo)")

   ;; Buffer
   "b"        '(nil :wk "Buffer")
   "bb"       '(bury-buffer :wk "Bury buffer")
   "bc"       '(clone-indirect-buffer :wk "Clone buffer")
   "bC"       '(clone-indirect-buffer-other-window
                :wk "Clone buffer other window")
   "bi"       '(ibuffer :wk "Open ibuffer")
   "bk"       '(nil :wk "Kill")
   "bkk"      '(kill-current-buffer :wk "Kill buffer")
   "bkK"      '(kill-buffer-and-window :wk "Kill buffer & window")
   "bkm"      '(kill-matching-buffers :wk "Kill matching buffers")
   "bko"      '(+kill-other-buffers :wk "Kill other file buffers")
   "bkO"      '(+kill-all-other-buffers :wk "Kill other buffers")
   "bks"      '(kill-some-buffers :wk "Kill some buffers")
   "bl"       '(evil-switch-to-windows-last-buffer :wk "Last buffer")
   "bn"       '(next-buffer :wk "Next buffer")
   "bp"       '(previous-buffer :wk "Previous buffer")
   "bN"       '(evil-buffer-new :wk "New buffer")
   "br"       '(revert-buffer :wk "Revert buffer")

   ;; Insert
   "I"        '(nil :wk "Insert")
   "If"       '(nil :wk "File")
   "Iff"      '(+insert-file-name :wk "File name")
   "Ifb"      '(+insert-file-name-base :wk "File base-name")
   "Ifd"      '(+insert-file-name-directory :wk "File directory")
   "IF"       '(add-file-local-variable-prop-line :wk "File Local Prop")
   "It"       '(nil :wk "time")
   "Iti"      '(+insert-iso-datetime :wk "ISO date/time")
   "Ito"      '(+insert-ordinal-date :wk "Ordinal date")
   "Itu"      '(+insert-unix-time :wk "UNIX timestamp")
   "Iu"       '(+insert-uuid :wk "UUID")
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
  (:states 'insert
   ;; Indent newlines by default
   [remap newline] 'newline-and-indent)
  (:keymaps 'evil-motion-state-map
   "RET" nil)
  :commands (evil-force-normal-state)
  :functions (+run-escape-hooks-a)
  :custom
  ;; Always indent when opening a new line.
  (evil-auto-indent t)
  ;; Allow horizontal motions to move to the next/previous line(s).
  (evil-cross-lines t)
  ;; We show the current state in the modeline, so no need to show it in the
  ;; echo area, too.
  (evil-echo-state nil)
  ;; Don't highlight searches in all buffers.
  (evil-ex-interactive-search-highlight 'selected-window)
  ;; Enable vim-style backslash codes.
  (evil-ex-search-vim-style-regexp t)
  ;; Use evil's own interactive search module.
  (evil-search-module 'evil-search)
  ;; The default is 4, but lots of languages only use a 2 step indentation, so
  ;; lower this to 2.
  (evil-shift-width 2)
  ;; Use undo-tree
  (evil-undo-system 'undo-tree)
  (evil-undo-function 'undo-tree-undo)
  (evil-redo-function 'undo-tree-redo)
  ;; We default some modes to emacs state, such as vterm, but we still want to
  ;; comfortably move around windows, so enable the C-w prefix.
  (evil-want-C-w-in-emacs-state t)
  ;; How characters are being interpreted when used in patters. Has the same
  ;; meaning as in vim:
  ;;
  ;; Some characters in the pattern are taken literally.  They match with the
  ;; same character in the text.  When preceded with a backslash however, these
  ;; characters get a special meaning.
  ;;
  ;; Other characters have a special meaning without a backslash.  They need to
  ;; be preceded with a backslash to match literally.
  ;;
  ;; If a character is taken literally or not depends on the "magic" option and
  ;; the items mentioned next.
  ;;
  ;; */\m* */\M*
  ;;
  ;; Use of "\m" makes the pattern after it be interpreted as if "magic" is
  ;; set, ignoring the actual value of the "magic" option.
  ;;
  ;; Use of "\M" makes the pattern after it be interpreted as if "nomagic" is
  ;; used.
  ;;
  ;; */\v* */\V*
  ;;
  ;; Use of "\v" means that in the pattern after it all ASCII characters except
  ;; "0"-"9", "a"-"z", "A"-"Z" and "_" have a special meaning.  "very magic"
  ;;
  ;; Use of "\V" means that in the pattern after it only the backslash has a
  ;; special meaning.  "very nomagic"
  ;; Examples:
  ;; after:       \v       \m       \M       \V         matches
  ;;            "magic" "nomagic"
  ;;              $        $        $        \$         matches end-of-line
  ;;              .        .        \.       \.         matches any character
  ;;              *        *        \*       \*         any number of the
  ;;                                                    previous atom
  ;;              ()       \(\)     \(\)     \(\)       grouping into an atom
  ;;              |        \|       \|       \|         separating alternatives
  ;;              \a       \a       \a       \a         alphabetic character
  ;;              \\       \\       \\       \\         literal backslash
  ;;              \.       \.       .        .          literal dot
  ;;              \{       {        {        {          literal "{"
  ;;              a        a        a        a          literal "a"
  ;;
  (evil-magic 'very-magic)
  ;; We bind lookups ourselves where we want them, so ignore the default.
  (evil-lookup-func #'ignore)
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

  (defun +sudo-find-file (file)
    "Open FILE as root."
    (interactive "FOpen file as root: ")
    (when (file-writable-p file)
      (user-error "File is user writeable, aborting sudo"))
    (find-file
     (if (file-remote-p file)
         (concat "/" (file-remote-p file 'method) ":"
                 (file-remote-p file 'user) "@" (file-remote-p file 'host)
                 "|sudo:root@"
                 (file-remote-p file 'host) ":" (file-remote-p file 'localname))
       (concat "/sudo:root@localhost:" file))))

  (defun +kill-buffer-or-delete-window ()
    "If more than one window is open, delete the current window, otherwise kill
current buffer."
    (interactive)
    (if (> (length (window-list)) 1)
        (delete-window)
      (kill-buffer)))

  (evil-ex-define-cmd "q" '+kill-buffer-or-delete-window)

  (defun +run-escape-hooks-a ()
    "Run ef-escape-hook hooks"
    (run-hooks 'ef-escape-hook))

  (advice-add 'evil-force-normal-state :after #'+run-escape-hooks-a))

(use-package evil-nerd-commenter
  :after evil
  :defer t
  :commands (evilnc-comment-or-uncomment-lines)
  :general
  ([remap comment-line] 'evilnc-comment-or-uncomment-lines)
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
  :functions (+undo-tree-save-history-a)
  :general
  (:states 'normal :prefix ef-leader
   "u" '(undo-tree-visualize :wk "Undo-tree"))
  :config
  (+add-hook undo-tree-visualizer-mode-hook
    (setq-local show-trailing-whitespace nil))

  (global-undo-tree-mode t)

  (defun +undo-tree-save-history-a (orig-fn &rest args)
    "Advice for `undo-tree-save-history' to hide echo area messages."
    (let ((message-log-max nil)
          (inhibit-message t))
      (apply orig-fn args)))

  (advice-add 'undo-tree-save-history :around #'+undo-tree-save-history-a))

(use-package evil-collection
  :after evil
  :demand t
  :commands (evil-collection-init)
  :config
  ;; Disable eglot bindings since we set them up ourselves and this one
  ;; overrides our keybinds set via general.
  (customize-set-variable 'evil-collection-mode-list
                          (remove 'eglot evil-collection-mode-list))
  (evil-collection-init))

(use-package anzu
  :after evil
  :demand t
  :commands (global-anzu-mode)
  :general
  ([remap query-replace] 'anzu-query-replace)
  ([remap query-replace-regexp] 'anzu-query-replace-regexp)
  :hook (ef-first-command . global-anzu-mode))

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
