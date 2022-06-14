(require 'core-evil)

(use-package dockerfile-mode
  :mode "Dockerfile\\'"
  :config
  (evil-define-key 'normal dockerfile-mode-map ",cc" #'dockerfile-build-buffer)
  (evil-define-key 'normal dockerfile-mode-map ",cb" #'dockerfile-build-no-cache-buffer)
  (evil-define-key 'normal dockerfile-mode-map ",tt" #'dockerfile-test-function))

(provide 'lang-dockerfile)
