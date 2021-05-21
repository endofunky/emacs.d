(use-package flyspell
  :custom
  (flyspell-issue-welcome-flag nil)
  (flyspell-issue-message-flag nil)
  :hook ((text-mode . flyspell-mode)))

(use-package ispell
  :hook (text-mode . ispell-minor-mode)
  :config
  (define-advice ispell-init-process
      (:around (old-fun &rest args) inhibit-message)
    (let ((inhibit-message t))
      (apply old-fun args))))

(use-package flyspell-correct
  :ensure t
  :after flyspell
  :general
  (:keymaps 'flyspell-mode-map
   "C-;" 'flyspell-correct-wrapper))

(provide 'core-spell)
