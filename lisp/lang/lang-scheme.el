(require 'core-evil)
(require 'core-lib)
(require 'core-shackle)

(use-package geiser
  :custom
  (geiser-active-implementations '(mit guile))
  :hook
  (scheme-mode . geiser-mode)
  :general
  (:prefix ef-local-leader :states 'normal :keymaps 'geiser-mode-map
   "c" '(nil :wk "Compile")
   "cc" '(geiser-compile-current-buffer :wk "Buffer")
   "cd" '(geiser-compile-definition :wk "Definition")

   "e" '(nil :wk "Eval")
   "eb" '(geiser-eval-buffer :wk "Buffer")
   "ed" '(geiser-eval-definition :wk "Definition")
   "es" '(geiser-eval-last-sexp :wk "S-exp")

   "m" '(nil :wk "Macro")
   "me" '(macrostep-expand :wk "Expand")
   "mE" '(macrostep-geiser-expand-all :wk "Expand all")
   "mq" '(macrostep-collapse-all :wk "Quit")

   "r" '(nil :wk "REPL")
   "rr" '(geiser-mode-switch-to-repl :wk "Open")
   "rq" '(geiser-repl-exit :wk "Quit")
   "rR" '(geiser-restart-repl :wk "Restart"))
  (:prefix ef-local-leader :states 'visual :keymaps 'geiser-mode-map
   "e" '(nil :wk "Eval")
   "er" '(geiser-eval-region :wk "Region"))
  (:states 'normal :keymaps 'geiser-doc-mode-map
   "n" 'forward-button
   "p" 'backward-button
   "N" 'geiser-doc-next-section
   "P" 'geiser-doc-previous-section)
  :defines (geiser-repl-mode)
  :config
  (require 'geiser-compile)
  (require 'geiser-doc)

  (evil-set-initial-state 'geiser-repl-mode 'normal)

  (+add-popup "*geiser*" :ephemeral t)
  (+add-popup "^\\*[gG]eiser" :regexp t :ephemeral t)
  (+add-popup "^\\*Geiser documentation\\*$" :regexp t :ephemeral t :size 0.4)
  (+add-popup  "^\\* [A-Za-z0-9_-]+ REPL \\*" :regexp t))

(use-package geiser-guile
  :after geiser)

(use-package geiser-mit
  :after geiser)

(use-package macrostep-geiser
  :after geiser
  :hook
  (geiser-mode . macrostep-geiser-setup)
  (geiser-repl-mode . macrostep-geiser-setup))

(provide 'lang-scheme)
