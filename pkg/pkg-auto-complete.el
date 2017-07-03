(use-package auto-complete
  :ensure t
  :diminish auto-complete-mode
  :commands (auto-complete-mode)
  :init
  (add-hook 'prog-mode-hook 'auto-complete-mode)
  :config
  (require 'auto-complete-config)
  (ac-set-trigger-key "TAB")
  (ac-config-default)

  (setq ac-auto-show-menu t)
  (setq ac-auto-start 2)
  (setq ac-candidate-limit 30)
  (setq ac-comphist-file  "~/.emacs.d/ac-comphist.dat")
  (setq ac-delay 0.01)
  (setq ac-dwim t)
  (setq ac-fuzzy-enable t)
  (setq ac-ignore-case nil)
  (setq ac-menu-height 10)
  (setq ac-quick-help-delay 0.5)
  (setq ac-use-menu-map t)
  (setq ac-use-quick-help t)

  ;; Workaround so the fringe doesn't flicker when completing close to EOF
  (use-package diff-hl
    :defer t
    :config
    (defadvice diff-hl-update (around ts/diff-hl-update-workaround activate)
      (unless ac-completing
        ad-do-it))))

(provide 'pkg-auto-complete)
