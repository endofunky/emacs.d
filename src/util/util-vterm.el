(require 'core-evil)
(require 'core-shackle)

(use-package vterm
  :when (bound-and-true-p module-file-suffix)
  :commands (vterm ef-vterm-popup ef-vterm-run)
  :custom
  (vterm-disable-bold-font nil)
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 10000)
  (vterm-timer-delay 0.01)
  :general
  (:states 'emacs :keymaps 'vterm-mode-map
   "M-h" 'ef-popup-cycle-backward
   "M-j" 'ef-popup-demote-buffer
   "M-k" 'ef-popup-promote-buffer
   "M-l" 'ef-popup-cycle-forward
   "M-P" 'ef-popup-switch-popup-buffer
   "M-p" 'ef-popup-toggle)
  (:states 'normal :prefix ef-prefix
   "v" '(ef-vterm-popup :wk "VTerm Popup"))
  :config
  (declare-function vterm-send-string "vterm")
  (declare-function ef--vterm-sentinel "util-vterm")
  (declare-function ef--vterm-sentinel-keep-buffer "util-vterm")

  (ef-add-hook vterm-mode-hook
    ;; Prevent premature horizontal scrolling
    (setq-local hscroll-margin 0)

    ;; Don't move the cursor back when entering evil-normal-state.
    (setq-local evil-move-cursor-back nil))

  (defun ef-vterm-popup ()
    "Open vterm popup buffer"
    (interactive)
    (if-let ((buf (get-buffer "*vterm-popup*")))
        (display-buffer "*vterm-popup*")
      (vterm "*vterm-popup*")))

  (ef-add-popup "*vterm-popup*" :size 0.4)

  (defun ef--vterm-sentinel (process event)
    "A process sentinel. Kills PROCESS's buffer if it is live."
    (let ((b (process-buffer process)))
      (when (buffer-live-p b)
        (kill-buffer b)
        (message "process finished."))))

  (defun ef--vterm-sentinel-keep-buffer (process event)
    "A process sentinel. Kills PROCESS's buffer if it is live."
    (let ((b (process-buffer process)))
      (when (buffer-live-p b)
        ;; Re-enable normal state so leader keybinds work.
        (evil-normal-state)
        (message "process finished."))))

  (defun ef-vterm-run (buffer command keep-buffer)
    (ef-add-popup buffer :size 0.4)

    (when-let ((buffer (get-buffer buffer)))
      (when-let ((win (get-buffer-window buffer)))
        (delete-window win))
      (kill-buffer buffer))

    (let ((vterm-shell (format "%s -c '%s'" vterm-shell command))
          (vterm-kill-buffer-on-exit nil))
      (with-current-buffer (vterm buffer)
        (if keep-buffer
            (set-process-sentinel vterm--process #'ef--vterm-sentinel-keep-buffer)
          (set-process-sentinel vterm--process #'ef--vterm-sentinel)))))

  ;; DOSBox colors
  (set-face-attribute 'vterm-color-black nil :foreground "#000000" :background "#545454")
  (set-face-attribute 'vterm-color-red nil :foreground "#a80000" :background "#fc5454")
  (set-face-attribute 'vterm-color-green nil :foreground "#00a800" :background "#54fc54")
  (set-face-attribute 'vterm-color-yellow nil :foreground "#a85400" :background "#fcfc54")
  (set-face-attribute 'vterm-color-blue nil :foreground "#0000a8" :background "#5454fc")
  (set-face-attribute 'vterm-color-magenta nil :foreground "#a800a8" :background "#fc54fc")
  (set-face-attribute 'vterm-color-cyan nil :foreground "#00a8a8" :background "#54fcfc")
  (set-face-attribute 'vterm-color-white nil :foreground "#a8a8a8" :background "#fcfcfc"))

(use-package evil-collection-vterm
  :after vterm
  :config
  (declare-function evil-collection-vterm-setup "evil-collection-vterm")
  (evil-collection-vterm-setup)

  (evil-set-initial-state 'vterm-mode 'emacs))

(provide 'util-vterm)