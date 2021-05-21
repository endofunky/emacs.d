(require 'core-lib)
(require 'core-shackle)

(use-package geiser
  :ensure t
  :commands (geiser-mode
             geiser
             geiser-mode-switch-to-repl
             geiser-eval-buffer
             geiser-eval-region
             geiser-eval-last-sexp)
  :custom
  (geiser-active-implementations '(mit guile))
  :hook
  (scheme-mode . geiser-mode)
  :general
  (:states 'normal :keymaps 'geiser-doc-mode-map
   "n" 'forward-button
   "p" 'backward-button
   "N" 'geiser-doc-next-section
   "P" 'geiser-doc-previous-section)
  :defines (geiser-repl-mode)
  :config
  (require 'geiser-company)
  (require 'geiser-compile)
  (require 'geiser-doc)

  (ef-add-popup "*geiser*" :ephemeral t)
  (ef-add-popup "^\\*[gG]eiser \\(dbg\\|xref\\|messages\\)\\*$" :regexp t :ephemeral t)
  (ef-add-popup "^\\*Geiser documentation\\*$" :regexp t :ephemeral t :size 0.4)
  (ef-add-popup  "^\\* [A-Za-z0-9_-]+ REPL \\*" :regexp t))

(use-package geiser-guile
  :after geiser
  :ensure t)

(use-package geiser-mit
  :after geiser
  :ensure t)

(use-package macrostep-geiser
  :after geiser
  :ensure t
  :hook
  (geiser-mode . macrostep-geiser-setup)
  (geiser-repl-mode . macrostep-geiser-setup))

(ef-deflang scheme
  :after geiser-mode
  :compile geiser-compile-current-buffer
  :compile-defun geiser-compile-definition
  :compile-nav-jump geiser-edit-symbol-at-point
  :compile-nav-pop-back geiser-pop-symbol-stack
  :compile-backend-connect geiser
  :compile-backend-reconnect geiser-restart-repl
  :compile-backend-quit geiser-repl-exit

  :doc-manual geiser-doc-module
  :doc-search geiser-doc-look-up-manual

  :eval-buffer geiser-eval-buffer
  :eval-defun geiser-eval-definition
  :eval-region geiser-eval-region
  :eval-sexp geiser-eval-last-sexp

  ;; macro
  :macro-expand-one macrostep-expand
  :macro-expand-all macrostep-geiser-expand-all
  :macro-quit macrostep-collapse-all

  :repl-context geiser-set-scheme
  :repl-toggle geiser-mode-switch-to-repl

  :xref-references geiser-xref-callers
  :xref-dependencies geiser-xref-callees)

(provide 'lang-scheme)
