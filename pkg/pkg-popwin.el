(use-package popwin
  :ensure t
  :config
  (popwin-mode 1)
  (add-to-list 'popwin:special-display-config
               '("*ag search*"
                 :dedicated t
                 :position bottom
                 :stick nil
                 :noselect nil
                 :height 0.4))

  (add-to-list 'popwin:special-display-config
               '("*Help*"
                 :dedicated t
                 :position bottom
                 :stick t
                 :noselect nil
                 :height 0.3))

  (add-to-list 'popwin:special-display-config
               '("*compilation*"
                 :dedicated t
                 :position bottom
                 :stick t
                 :noselect t
                 :height 0.3))

  (add-to-list 'popwin:special-display-config
               '("*Shell Command Output*"
                 :dedicated t
                 :position bottom
                 :stick t
                 :noselect nil))

  (add-to-list 'popwin:special-display-config
               '("*Async Shell Command*"
                 :dedicated t
                 :position bottom
                 :stick t
                 :noselect nil))

  (add-to-list 'popwin:special-display-config
               '(" *undo-tree*"
                 :dedicated t
                 :position bottom
                 :stick t
                 :noselect nil
                 :height 0.3)))

(provide 'pkg-popwin)
