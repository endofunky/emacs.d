;;; core-eldoc.el --- ElDoc configuration -*- lexical-binding: t; -*-
(require 'core-evil)

(use-package eldoc
  :defer t
  :custom
  (eldoc-idle-delay 0.5)
  ;; Limit eldoc documentation in echo area to 1 line.
  (eldoc-echo-area-use-multiline-p 1)
  :config
  ;; Eldoc massively slows down cursor movement. This advice fixes that.
  (advice-add 'eldoc-pre-command-refresh-echo-area :override #'ignore)

  ;; Trigger `eldoc' after changing evil states
  (eldoc-add-command 'evil-normal-state
                     'evil-insert
                     'evil-change
                     'evil-delete
                     'evil-replace))

(provide 'core-eldoc)
