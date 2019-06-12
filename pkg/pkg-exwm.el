(use-package exwm
  :demand t
  :if (equal (system-name) "xor")
  :custom
  (exwm-input-line-mode-passthrough t)
  (exwm-workspace-number 6)
  :config
  (defun ef-exwm-workspace-next ()
    (interactive)
    (when (< (exwm-workspace--position exwm-workspace--current)
             (- (exwm-workspace--count) 1))
      (exwm-workspace-switch
       (+ (exwm-workspace--position exwm-workspace--current) 1))))

  (defun ef-exwm-workspace-prev ()
    (interactive)
    (when (> (exwm-workspace--position exwm-workspace--current) 0)
      (exwm-workspace-switch
       (- (exwm-workspace--position exwm-workspace--current) 1))))

  (defun ef-exwm-run-shell-command (command)
    (interactive (list (read-shell-command "$ ")))
    (start-process-shell-command command nil command))

  (customize-set-variable
   'exwm-input-global-keys
   `(([?\s-r] . exwm-reset)
     ([?\s-&] . ef-exwm-run-shell-command)
     (,(kbd "<XF86AudioMute>") . pulseaudio-control-toggle-current-sink-mute)
     (,(kbd "<XF86AudioMicMute>") . pulseaudio-control-toggle-current-source-mute)
     (,(kbd "<XF86AudioLowerVolume>") . pulseaudio-control-decrease-volume)
     (,(kbd "<XF86AudioRaiseVolume>") . pulseaudio-control-increase-volume)
     (,(kbd "<XF86MonBrightnessDown>") . backlight-dec)
     (,(kbd "<XF86MonBrightnessUp>") . backlight-inc)
     (,(kbd "C-M-l") . ef-exwm-workspace-next)
     (,(kbd "C-M-h") . ef-exwm-workspace-prev)
     (,(kbd "<print>") . (ef-wrap-shell-command "scrot -e 'mv $f ~/media/images/'"))
     (,(kbd "M-<print>") . (ef-wrap-shell-command "scrot -s -e 'mv $f ~/media/images/'"))))

  (evil-define-key 'normal global-map
    ",eL" (ef-wrap-shell-command "i3lock -e -c 000000 --nofork")
    ",ef" (ef-wrap-shell-command "google-chrome-stable")
    ",es" (ef-wrap-shell-command "spotify")
    ",ex" (ef-wrap-shell-command "xterm"))

  (evil-define-key 'normal exwm-mode-map (kbd "i") 'exwm-input-release-keyboard)

  (ef-add-hook exwm-update-class-hook
    (exwm-workspace-rename-buffer exwm-class-name))

  (ef-add-hook exwm-manage-finish-hook
    (call-interactively #'exwm-input-release-keyboard))

  (defadvice exwm-input-grab-keyboard (after ef activate)
    (evil-normal-state))

  (defadvice exwm-input-release-keyboard (after ef activate)
    (evil-insert-state))

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
  :ensure t
  :commands (pulseaudio-control-increase-volume
             pulseaudio-control-decrease-volume
             pulseaudio-control-toggle-current-source-mute
             pulseaudio-control-toggle-current-sink-mute))

(use-package backlight
  :ensure t
  :commands (backlight-inc
             backlight-dec))

(provide 'pkg-exwm)
