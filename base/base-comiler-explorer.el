(require 'core-evil)

(use-package compiler-explorer
  :ensure t
  :general
  (:states 'normal :prefix ef-prefix
           "cE" 'compiler-explorer)
  :functions compiler-explorer-new-session
  :commands (compiler-explorer
             compiler-explorer-new-session))

(provide 'base-compiler-explorer)
