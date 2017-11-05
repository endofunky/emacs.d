(use-package smerge-mode
  :after evil
  :defer t
  :commands smerge-mode
  :config
  (evil-define-key 'normal smerge-mode-map ",sc" 'smerge-keep-current)
  (evil-define-key 'normal smerge-mode-map ",sn" 'smerge-next)
  (evil-define-key 'normal smerge-mode-map ",sp" 'smerge-previous))

(provide 'pkg-smerge)
