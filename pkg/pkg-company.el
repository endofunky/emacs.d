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
  (setq ac-delay 0.01)
  (setq ac-use-menu-map t)
  (setq ac-menu-height 10)
  (setq ac-use-quick-help t)
  (setq ac-comphist-file  "~/.emacs.d/ac-comphist.dat")
  (setq ac-ignore-case nil)
  (setq ac-fuzzy-enable t)
  (setq ac-candidate-limit 30)
  (setq ac-dwim t))

(provide 'pkg-company)
