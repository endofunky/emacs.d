;;; lang-vterm.el --- VTerm terminal configuration -*- lexical-binding: t; -*-
(require 'core-evil)
(require 'core-popup)

(use-package vterm
  :when (bound-and-true-p module-file-suffix)
  :commands (vterm +vterm-popup +vterm-run)
  :functions (+vterm-sentinel-keep-buffer
              +vterm-sentinel)
  :custom
  (vterm-disable-bold-font nil)
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 10000)
  ;; Refresh at roughly 60 fps.
  (vterm-timer-delay 0.016)
  :general
  (:states 'emacs :keymaps 'vterm-mode-map
   "M-h" 'poe-popup-next
   "M-l" 'poe-popup-prev
   "M-p" 'poe-popup-toggle
   "M-q" 'poe-popup-kill)
  (:states 'normal :prefix ef-leader
   "v" '(+vterm-popup :wk "VTerm")
   "V" '(+vterm-popup-and-go :wk "VTerm & go"))
  :config
  (declare-function vterm-send-string "vterm")
  (declare-function vterm--internal "vterm")

  (+add-hook vterm-mode-hook
    ;; Prevent premature horizontal scrolling
    (setq-local hscroll-margin 0)

    ;; Don't move the cursor back when entering evil-normal-state.
    (setq-local evil-move-cursor-back nil))

  (defun +vterm-popup ()
    "Open vterm popup buffer"
    (interactive)
    (if-let ((buf (get-buffer "*vterm-popup*")))
        (display-buffer "*vterm-popup*")
      (vterm--internal #'ignore "*vterm-popup*")
      (display-buffer "*vterm-popup*")))

  (defun +vterm-popup-and-go ()
    "Go to current file's directory in vterm popup."
    (interactive)
    (if-let* ((file (buffer-file-name (current-buffer)))
              (dir (file-name-directory file)))
        (if-let ((buf (get-buffer "*vterm-popup*")))
            (progn
              (display-buffer "*vterm-popup*")
              (with-current-buffer buf
                (vterm-send-string (concat "cd " dir))))
          (vterm "*vterm-popup*"))

      (message "Buffer is not visiting a file.")))

  (poe-popup "*vterm-popup*" :size 0.4 :select t)

  (defun +vterm-sentinel (process _event)
    "A process sentinel. Kills PROCESS's buffer if it is live."
    (let ((b (process-buffer process)))
      (when (buffer-live-p b)
        (kill-buffer b)
        (message "process finished."))))

  (defun +vterm-sentinel-keep-buffer (process _event)
    "A process sentinel. Kills PROCESS's buffer if it is live."
    (let ((b (process-buffer process)))
      (when (buffer-live-p b)
        ;; Re-enable normal state so leader keybinds work.
        (evil-normal-state)
        (message "process finished."))))

  (defun +vterm-run (buffer command keep-buffer)
    (poe-popup buffer :size 0.4)

    (when-let ((buffer (get-buffer buffer)))
      (when-let ((win (get-buffer-window buffer)))
        (delete-window win))
      (kill-buffer buffer))

    (let ((vterm-shell (format "%s -c '%s'" vterm-shell command))
          (vterm-kill-buffer-on-exit nil))
      (with-current-buffer (vterm buffer)
        (if keep-buffer
            (set-process-sentinel vterm--process #'+vterm-sentinel-keep-buffer)
          (set-process-sentinel vterm--process #'+vterm-sentinel)))))

  ;; EGA color palette
  (set-face-attribute 'vterm-color-black nil
                      :foreground "#000000" :background "#555555")
  (set-face-attribute 'vterm-color-blue nil
                      :foreground "#0000aa" :background "#5555ff")
  (set-face-attribute 'vterm-color-green nil
                      :foreground "#00aa00" :background "#55ff55")
  (set-face-attribute 'vterm-color-cyan nil
                      :foreground "#00aaaa" :background "#55ffff")
  (set-face-attribute 'vterm-color-red nil
                      :foreground "#aa0000" :background "#ff5555")
  (set-face-attribute 'vterm-color-magenta nil
                      :foreground "#aa00aa" :background "#ff55ff")
  (set-face-attribute 'vterm-color-yellow nil
                      :foreground "#aa5500" :background "#ffff55")
  (set-face-attribute 'vterm-color-white nil
                      :foreground "#aaaaaa" :background "#ffffff"))

(use-package evil-collection-vterm
  :straight nil
  :after vterm
  :config
  (declare-function evil-collection-vterm-setup "evil-collection-vterm")
  (evil-collection-vterm-setup)

  (evil-set-initial-state 'vterm-mode 'emacs))

(provide 'util-vterm)
