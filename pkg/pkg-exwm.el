(use-package exwm
  :demand t
  :if (equal (system-name) "xor")
  :custom
  (exwm-input-line-mode-passthrough t)
  :config
  (require 'exwm-config)
  (exwm-config-default)

  (defmacro ef-exwm-shell-command (key cmd)
    (let ((fn `(lambda ()
                 (interactive)
                 (start-process-shell-command ,(car (split-string cmd)) nil ,cmd))))
      (if (stringp key)
          `(define-key evil-normal-state-map ,key ,fn)
        `(exwm-input-set-key ,key ,fn))))

  (defun ef-exwm-workspace-next ()
    (interactive)
    (when (< (exwm-workspace--position exwm-workspace--current)
             (- (exwm-workspace--count) 1))
      (ef-flycheck-close-window)
      (exwm-workspace-switch
       (+ (exwm-workspace--position exwm-workspace--current) 1))))

  (defun ef-exwm-workspace-prev ()
    (interactive)
    (when (> (exwm-workspace--position exwm-workspace--current) 0)
      (ef-flycheck-close-window)
      (exwm-workspace-switch
       (- (exwm-workspace--position exwm-workspace--current) 1))))

  (exwm-input-set-key (kbd "C-M-l") #'ef-exwm-workspace-next)
  (exwm-input-set-key (kbd "C-M-h") #'ef-exwm-workspace-prev)

  (ef-exwm-shell-command ",eL" "i3lock -e -c 000000 --nofork")
  (ef-exwm-shell-command ",ef" "google-chrome-stable")
  (ef-exwm-shell-command ",es" "spotify")
  (ef-exwm-shell-command ",ex" "xterm")
  (ef-exwm-shell-command (kbd "<print>") "scrot -e 'mv $f ~/media/images/'")
  (ef-exwm-shell-command (kbd "M-<print>") "scrot -s -e 'mv $f ~/media/images/'")

  (ef-add-hook exwm-manage-finish-hook
    (call-interactively #'exwm-input-release-keyboard))

  (defadvice exwm-input-grab-keyboard (after ef activate)
    (evil-normal-state))

  (defadvice exwm-input-release-keyboard (after ef activate)
    (evil-insert-state))

  (evil-define-key 'normal exwm-mode-map (kbd "i") 'exwm-input-release-keyboard)
  (push ?\i exwm-input-prefix-keys)
  (exwm-init))

(use-package battery
  :after exwm
  :custom
  (battery-mode-line-format "⚡ %b%p%% ")
  :config
  (display-battery-mode t))

(use-package time
  :after exwm
  :custom
  (display-time-default-load-average nil)
  :config
  (display-time-mode t))

(use-package pulseaudio-control
  :after exwm
  :ensure t
  :commands (pulseaudio-control-increase-volume
             pulseaudio-control-decrease-volume
             pulseaudio-control-toggle-current-source-mute
             pulseaudio-control-toggle-current-sink-mute)
  :init
  (exwm-input-set-key (kbd "<XF86AudioMute>") #'pulseaudio-control-toggle-current-sink-mute)
  (exwm-input-set-key (kbd "<XF86AudioMicMute>") #'pulseaudio-control-toggle-current-source-mute)
  (exwm-input-set-key (kbd "<XF86AudioLowerVolume>") #'pulseaudio-control-decrease-volume)
  (exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") #'pulseaudio-control-increase-volume))

(use-package backlight
  :ensure t
  :after exwm
  :commands (backlight-inc
             backlight-dev)
  :config
  (exwm-input-set-key (kbd "<XF86MonBrightnessUp>") #'backlight-inc)
  (exwm-input-set-key (kbd "<XF86MonBrightnessDown>") #'backlight-dec))

(provide 'pkg-exwm)
