(use-package exwm
  :demand t
  :if (equal (system-name) "xor")
  :custom
  (exwm-input-line-mode-passthrough t)
  :config
  (require 'exwm-config)
  (exwm-config-default)

  (add-hook 'exwm-manage-finish-hook (lambda () (call-interactively #'exwm-input-release-keyboard)))

  (defadvice exwm-input-grab-keyboard (after ef activate)
    (evil-normal-state))

  (defadvice exwm-input-release-keyboard (after ef activate)
    (evil-insert-state))

  ;; In normal state/line mode, use the familiar i key to switch to input state
  (evil-define-key 'normal exwm-mode-map (kbd "i") 'exwm-input-release-keyboard)
  (push ?\i exwm-input-prefix-keys)
  (push ?\, exwm-input-prefix-keys)
  (exwm-init)
  (display-time-mode t)
  (display-battery-mode t))

(provide 'pkg-exwm)
