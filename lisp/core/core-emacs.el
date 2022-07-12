;;; core-emacs.el --- Built-in configurations -*- lexical-binding: t; -*-
(require 'core-lib)

(+csetq
 ;; Display keystrokes in echo area as soon as possible.
 ;; Note: Setting this to a value < 0 has the same effect as setting it to
 ;; 0 since (at least) Emacs 25, disabling it completely.
 echo-keystrokes 0.0001

 ;; We don't want tabs in most cases. Where we do, we will enable them
 ;; explicitly.
 indent-tabs-mode nil

 ;; Disable bell completely.
 ring-bell-function 'ignore

 ;; Scroll one line at a time.
 scroll-margin 0
 scroll-conservatively 100000
 scroll-preserve-screen-position 1

 ;; Empty scratch buffer by default.
 initial-scratch-message ""

 ;; Do less advertising during start-up.
 inhibit-splash-screen t
 inhibit-startup-message t

 ;; By default this is `elisp-mode', but we use `persistent-scratch' to save
 ;; the contents together with any buffer locals. So for the first start-up,
 ;; just use `fundamental-mode' to speed things up a little.
 initial-major-mode 'fundamental-mode

 ;; When we break lines we want to do so at 80 columns.
 fill-column 80

 ;; Don't wrap long lines by default. In modes where we want this, we will
 ;; enable it explicitly.
 truncate-lines -1

 ;; Word wrap at spaces.
 word-wrap t

 ;; Only respect file local variable specifications for variables considered
 ;; safe. Ignore the rest.
 enable-local-variables :safe

 ;; Hide commands in M-x which do not work in the current mode.
 read-extended-command-predicate #'command-completion-default-include-p

 ;; Don't show GUI dialog boxes.
 use-dialog-box nil

 ;; The Emacs >= 28 way of setting:
 ;; (fset 'yes-or-no-p #'y-or-n-p)
 use-short-answers t

 ;; Consider a period followed by a single space to be end of sentence.
 sentence-end-double-space nil

 ;; To quote the Linux kernel coding style:
 ;;
 ;;     Tabs are 8 characters, and thus indentations are also 8 characters.
 ;;     There are heretic movements that try to make indentations 4 (or even 2!)
 ;;     characters deep, and that is akin to trying to define the value of PI to
 ;;     be 3.
 ;;
 tab-width 8

 ;; Do not move the current file while creating backup.
 backup-by-copying t

 ;; Resize windows proportionally instead of taking space from the current window
 ;; only. This also ensures windows get rebalanced when a split gets closed.
 window-combination-resize t

 ;; Hide the cursor if the window loses focus
 cursor-in-non-selected-windows nil)

;; No GNU ads in minibuffer after start-up.
(fset #'display-startup-echo-area-message #'ignore)

;; Never use TUI pagers in sub-processes.
(setenv "PAGER" (executable-find "cat"))

;; Show columns in mode-line
(column-number-mode t)

(use-package ansi-color
  :after comint
  :straight nil
  :custom
  (ansi-color-for-comint-mode t)
  ;; Do not break markers in a buffer upon reverting a buffer. Details:
  ;; https://github.com/magit/magit/issues/4442
  (revert-buffer-insert-file-contents-function
   'revert-buffer-insert-file-contents-delicately)
  :defer t)

(use-package autorevert
  :straight nil
  :custom
  ;; Check files every second.
  (auto-revert-interval 1)
  ;; Don't spam the echo area with messages when reverting.
  (auto-revert-verbose nil)
  ;; Make autorevert version control aware.
  (auto-revert-check-vc-info t)
  ;; Refresh non-file buffers that implement autorevert.
  (global-auto-revert-non-file-buffers t)
  :config
  (global-auto-revert-mode 1))

(use-package calc
  :defer t
  :straight nil
  :custom
  (math-additional-units
   '((GiB "1024 * MiB" "Giga Byte")
     (MiB "1024 * KiB" "Mega Byte")
     (KiB "1024 * B" "Kilo Byte")
     (B nil "Byte")
     (Gib "1024 * Mib" "Giga Bit")
     (Mib "1024 * Kib" "Mega Bit")
     (Kib "1024 * b" "Kilo Bit")
     (b "B / 8" "Bit"))))

(use-package comint
  :defer t
  :straight nil
  :custom
  (comint-buffer-maximum-size 2048)
  (comint-move-point-for-output t)
  (comint-prompt-read-only t)
  :commands (comint-truncate-buffer)
  :hook (compilation-filter . comint-truncate-buffer)
  :general
  (:states 'insert :keymaps 'comint-mode-map
   "<up>" 'comint-previous-input
   "<down>" 'comint-next-input)
  :config
  (evil-set-initial-state 'comint-mode 'normal)
  (+add-hook comint-mode-hook
    "Do not use `truncate-lines' in comint buffers.."
    (setq-local truncate-lines nil)
    (set (make-local-variable 'truncate-partial-width-windows) nil)))

(use-package compile
  :defer t
  :straight nil
  :custom
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  (compilation-message-face 'default)
  (compilation-scroll-output 'first-error)
  :functions (ef-compilation-exit-autoclose)
  :hook
  ;; Avoid dropping into insert mode in compilation windows
  (compilation-start . evil-normal-state)
  (compilation-filter . comint-truncate-buffer)
  (compilation-filter . +colorize-compilation-buffer)
  :config
  (require 'ansi-color)
  (defun +colorize-compilation-buffer ()
    (read-only-mode nil)
    (ansi-color-apply-on-region compilation-filter-start (point))
    (read-only-mode t))

  (+add-hook compilation-mode-hook
    "Enable `visual-line-mode' for compilation buffers."
    (setq-local bidi-display-reordering nil)
    (visual-line-mode t)))

;; Type over region: Treat an Emacs region much like a typical selection
;; outside of Emacs.
(use-package delsel
  :straight nil
  :hook (after-init . delete-selection-mode))

(use-package display-line-numbers
  :straight nil
  :commands (display-line-numbers-mode)
  :custom
  (display-line-numbers-width-start t))

(use-package files
  :straight nil
  :unless noninteractive
  :general
  ("C-x C" '(restart-emacs :wk "Restart emacs"))
  :config
  (setq save-silently t)
  (+add-hook after-save-hook :fn +after-save-message-h
    (message "\"%s\" %dL, %dC written"
	     (buffer-name)
	     (count-lines (point-min) (point-max))
	     (buffer-size))))

(use-package hl-line
  :straight nil
  :defer t
  :custom
  (hl-line-sticky-flag nil)
  :hook
  (prog-mode . hl-line-mode)
  (text-mode . hl-line-mode)
  (conf-mode . hl-line-mode)
  (dired-mode . hl-line-mode)
  (tabulated-list-mode . hl-line-mode)
  (evil-visual-state-entry . +evil-disable-hl-line)
  (evil-visual-state-exit . +evil-enable-hl-line)
  (activate-mark-hook . +evil-disable-hl-line)
  (deactivate-mark-hook . +evil-enable-hl-line)
  :config
  ;; Fix bug with sometimes the current line not being highlighted until the
  ;; cursor is being moved when certain windows get closed.
  (defun +highlight-current-line (&rest args)
    "Hook function to re-highlight the current line if
`hl-line-mode-is-enabled'."
    (when (and (bound-and-true-p hl-line-mode)
               (fboundp 'hl-line-highlight))
      (hl-line-highlight)))

  (add-hook 'window-buffer-change-functions #'+highlight-current-line)

  ;; Disable hl-line when in visual state
  (defvar ef--hl-line-mode nil
    "Whether to re-enable hl-line if it was previously disabled while in evil
visual state or mark.")

  (defun +evil-disable-hl-line ()
    "Hook function to temporarily disable hl-line for visual state and mark."
    (when hl-line-mode
      (hl-line-mode -1)
      (setq ef--hl-line-mode t)))

  (defun +evil-enable-hl-line ()
    "Hook function to temporarily disable hl-line for visual state and mark."
    (when ef--hl-line-mode
      (hl-line-mode t))))

(use-package hideshow
  :straight nil
  :commands hs-minor-mode
  :hook (prog-mode . hs-minor-mode))

(use-package minibuffer
  :straight nil
  :custom
  (enable-recursive-minibuffers t)
  ;; Prevent the minibuffer prompt from being modified.
  (minibuffer-prompt-properties
   '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
  :hook
  (minibuffer-setup . cursor-intangible-mode)
  :general
  ;; Escape minibuffer with single escape.
  (:keymaps '(minibuffer-local-map
              minibuffer-local-ns-map
              minibuffer-local-completion-map
              minibuffer-local-must-match-map
              minibuffer-local-isearch-map)
   [escape] 'minibuffer-keyboard-quit)
  :config
  ;; This is where `minibuffer-keyboard-quit' is defined.
  (require 'delsel)

  (+add-hook minibuffer-setup-hook
    ;; Don't display fringes in minibuffer
    (set-window-fringes (get-buffer-window (current-buffer)) 0 0)))

(use-package prog-mode
  :straight nil
  :general
  (:states 'normal :keymaps 'prog-mode-map
   "<tab>" 'indent-for-tab-command)
  (:states 'normal :prefix ef-leader :keymaps 'prog-mode-map
   "<tab>" '(+indent-buffer :wk "Indent buffer"))
  (:states 'visual :keymaps 'prog-mode-map
   "<tab>" 'indent-region)
  :config
  (+add-hook prog-mode-hook
    (setq-local show-trailing-whitespace t))

  (global-prettify-symbols-mode -1))

(use-package savehist
  :straight nil
  :custom
  (history-length 1000)
  (savehist-additional-variables '(search ring regexp-search-ring))
  (savehist-autosave-interval 60)
  (savehist-save-minibuffer-history t)
  (savehist-additional-variables '(kill-ring
                                   register-alist
                                   mark-ring global-mark-ring
                                   search-ring
                                   regexp-search-ring
                                   extended-command-history))
  :hook (ef-first-command . savehist-mode))

(use-package saveplace
  :straight nil
  :custom
  (save-place-limit nil)
  :init
  (save-place-mode t)
  :config
  (setq-default save-place t))

(use-package smerge-mode
  :straight nil
  :commands smerge-mode
  :custom
  (smerge-command-prefix (kbd "C-s"))
  :init
  (+add-hook (find-file-hook after-revert-hook) :fn +enable-smerge-maybe-h
    "Auto-enable `smerge-mode' when merge conflict is detected."
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil :noerror)
        (smerge-mode t)))))

(use-package so-long
  :straight nil
  :config
  (global-so-long-mode 1))

(use-package tabify
  :straight nil
  :defer t
  :config
  (setq tabify-regexp "^\t* [ \t]+"))

(use-package text-mode
  :straight nil
  :defer t
  :mode (("/LICENSE\\'" . text-mode)
         ("\\.log\\'" . text-mode))
  :config
  (+add-hook text-mode-hook
    (setq-local show-trailing-whitespace t)))

(use-package transient
  :general
  (:keymaps '(transient-map transient-edit-map)
   "<escape>" 'transient-quit-all
   "?" 'transient-show
   "C-h" 'transient-show
   "C-t" 'transient-help)
  :commands (transient-define-prefix
              transient-bind-q-to-quit)
  :custom
  (transient-enable-popup-navigation t)
  (transient-show-popup t)
  :config
  (transient-bind-q-to-quit))

(use-package uniquify
  :straight nil
  :custom
  (uniquify-after-kill-buffer-p t)
  (uniquify-buffer-name-style 'post-forward-angle-brackets)
  (uniquify-ignore-buffers-re "^\\*")
  (uniquify-separator ":"))

(use-package vc
  :straight nil
  :defer t
  :straight nil
  :custom
  (vc-follow-symlinks t))

(use-package xref
  :straight nil
  :defer t
  :custom
  (xref-marker-ring-length 1024)
  :config
  (poe-popup "*xref*")

  (defadvice xref-goto-xref (after my activate)
    (delete-window (get-buffer-window (get-buffer "*xref*")))))

;; Enable mouse in xterm.
(use-package xt-mouse
  :straight nil
  :if (and (not (display-graphic-p))
           (string-prefix-p "xterm" (getenv "TERM")))
  :hook (after-init . xterm-mouse-mode)
  :config
  ;; Scrolling with the mouse should move text.
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

;; Auto-tail the *Messages* buffer
;;
;; From: https://stackoverflow.com/a/6341139/3058915
;; And: https://abdelhakbougouffa.pro/posts/config/
(defvar +messages-buffer-auto-tail--enabled nil)

(defun +messages-buffer-auto-tail--advice (&rest _args)
  "Make *Messages* buffer auto-scroll to the end after each message."
  (let* ((buf-name (buffer-name (messages-buffer)))
         ;; Create *Messages* buffer if it does not exist
         (buf (get-buffer-create buf-name)))
    ;; Activate this advice only if the point is _not_ in the *Messages* buffer
    ;; to begin with. This condition is required; otherwise you will not be
    ;; able to use `isearch' and other stuff within the *Messages* buffer as
    ;; the point will keep moving to the end of buffer :P
    (when (not (string= buf-name (buffer-name)))
      ;; Go to the end of buffer in all *Messages* buffer windows that are
      ;; *live* (`get-buffer-window-list' returns a list of only live windows).
      (dolist (win (get-buffer-window-list buf-name nil :all-frames))
        (with-selected-window win
          (goto-char (point-max))))
      ;; Go to the end of the *Messages* buffer even if it is not in one of
      ;; the live windows.
      (with-current-buffer buf
        (goto-char (point-max))))))

(defun +messages-buffer-toggle-auto-tail ()
  "Auto tail the '*Messages*' buffer."
  (interactive)
  ;; Add/remove an advice from the 'message' function.
  (cond (+messages-buffer-auto-tail--enabled
         (advice-remove 'message #'+messages-buffer-auto-tail--advice)
         (setq +messages-buffer-auto-tail--enabled nil)
         (message "+messages-buffer-auto-tail: Disabled."))
        (t
         (advice-add 'message :after #'+messages-buffer-auto-tail--advice)
         (setq +messages-buffer-auto-tail--enabled t)
         (message "+messages-buffer-auto-tail: Enabled."))))

(add-hook 'after-init-hook #'+messages-buffer-toggle-auto-tail)

(provide 'core-emacs)
