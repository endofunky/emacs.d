(use-package restclient
  :commands (restclient-mode)
  :config
  (when (featurep 'popwin)
    (add-to-list 'popwin:special-display-config
                 '("*HTTP Response*" :height 30 :dedicated t))))

(provide 'pkg-restclient)
