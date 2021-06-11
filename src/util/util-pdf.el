(use-package pdf-tools
  :ensure t
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :commands (pdf-view-mode pdf-tools-install)
  :custom
  (pdf-view-display-size 'fit-height)
  (pdf-annot-activate-created-annotations t)
  :hook
  (pdf-view-mode . pdf-view-midnight-minor-mode)
  :config
  (require 'pdf-tools)
  (require 'pdf-occur)
  (require 'pdf-history)
  (require 'pdf-links)
  (require 'pdf-outline)
  (require 'pdf-annot)
  (require 'pdf-sync)

  (pdf-tools-install :no-query)

  (general-define-key
   :states '(normal insert) :keymaps 'pdf-view-mode-map
   "q" 'kill-current-buffer))

(provide 'util-pdf)
