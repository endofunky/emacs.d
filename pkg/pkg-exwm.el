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

  (evil-define-key 'normal exwm-mode-map (kbd "i") 'exwm-input-release-keyboard)
  (push ?\i exwm-input-prefix-keys)
  (push ?\, exwm-input-prefix-keys)
  (exwm-init)
  (display-time-mode t)
  (display-battery-mode t))

(use-package pulseaudio-control
  :commands (pulseaudio-control-increase-volume
             pulseaudio-control-decrease-volume
             pulseaudio-control-toggle-current-source-mute
             pulseaudio-control-toggle-current-sink-mute)
  :after exwm
  :ensure t
  :init
  (exwm-input-set-key (kbd "<XF86AudioMute>") #'pulseaudio-control-toggle-current-sink-mute)
  (exwm-input-set-key (kbd "<XF86AudioMicMute>") #'pulseaudio-control-toggle-current-source-mute)
  (exwm-input-set-key (kbd "<XF86AudioLowerVolume>") #'pulseaudio-control-decrease-volume)
  (exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") #'pulseaudio-control-increase-volume))

(provide 'pkg-exwm)
