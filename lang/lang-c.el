(use-package cc-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.h$" . c-mode))
  :config
  (setq c-default-style "k&r")
  (setq c-basic-offset 4)
  (setq company-idle-delay 0.08)
  (setq company-minimum-prefix-length 2)

  ;; Reset backends and build it from scratch.
  (setq company-backends ())

  (use-package irony
    :ensure t
    :defer t
    :commands (irony-mode)
    :diminish irony-mode
    :init
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'c++-mode-hook 'irony-mode))

  (use-package company-irony
    :ensure t
    :commands (company-irony company-irony-setup-begin-commands)
    :init
    (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
    :config
    (add-to-list 'company-backends 'company-irony t))

  (use-package company-c-headers
    :defer t
    :ensure t
    :commands (company-c-headers)
    :init
    (add-to-list 'company-backends 'company-c-headers)))

(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

(provide 'lang-c)
