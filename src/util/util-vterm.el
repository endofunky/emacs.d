(require 'core-evil)
(require 'core-shackle)

(use-package vterm
  :when (bound-and-true-p module-file-suffix)
  :commands (vterm ef-vterm-popup)
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
   "v" '(ef-vterm-popup :wk "VTerm Popup")
   "V" '(ef-vterm-popup-and-go :wk "VTerm Popup & Go"))
  :hook
  (vterm-mode . evil-emacs-state)
  :config
  (declare-function vterm-send-string "vterm")

  (ef-add-hook vterm-mode-hook
    ;; Prevent premature horizontal scrolling
    (setq-local hscroll-margin 0))

  (defun ef-vterm-popup ()
    "Open vterm popup buffer"
    (interactive)
    (if-let ((buf (get-buffer "*vterm-popup*")))
        (display-buffer "*vterm-popup*")
      (vterm "*vterm-popup*")))

  (defun ef-vterm-popup-and-go ()
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

  (ef-add-popup "*vterm-popup*" :size 0.4)

  ;; DOSBox colors
  (set-face-attribute 'vterm-color-black nil :background "#000000" :foreground "#545454")
  (set-face-attribute 'vterm-color-red nil :background "#a80000" :foreground "#fc5454")
  (set-face-attribute 'vterm-color-green nil :background "#00a800" :foreground "#54fc54")
  (set-face-attribute 'vterm-color-yellow nil :background "#a85400" :foreground "#fcfc54")
  (set-face-attribute 'vterm-color-blue nil :background "#0000a8" :foreground "#5454fc")
  (set-face-attribute 'vterm-color-magenta nil :background "#a800a8" :foreground "#fc54fc")
  (set-face-attribute 'vterm-color-cyan nil :background "#00a8a8" :foreground "#54fcfc")
  (set-face-attribute 'vterm-color-white nil :background "#a8a8a8" :foreground "#fcfcfc"))

(use-package evil-collection-vterm
  :after vterm
  :config
  (declare-function evil-collection-vterm-setup "evil-collection-vterm")
  (evil-collection-vterm-setup))

(provide 'util-vterm)
