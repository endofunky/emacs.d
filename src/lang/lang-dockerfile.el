(use-package dockerfile-mode
  :straight t
  :mode "Dockerfile\\'"
  :config
  (evil-define-key 'normal dockerfile-mode-map ",cc" #'dockerfile-build-buffer)
  (evil-define-key 'normal dockerfile-mode-map ",cb" #'dockerfile-build-no-cache-buffer)
  (evil-define-key 'normal dockerfile-mode-map ",tt" #'dockerfile-test-function))

(provide 'lang-dockerfile)
