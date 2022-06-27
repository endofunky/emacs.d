(require 'core-evil)

(use-package dockerfile-mode
  :mode "Dockerfile\\'"
  :general
  (:prefix ef-local-leader :states 'normal :keymaps 'dockerfile-mode-map
   "c" '(nil :wk "Compile")
   "cc" '(dockerfile-build-buffer :wk "Build")
   "cC" '(dockerfile-build-no-cache-buffer :wk "Build (no cache)")

   "t" '(nil :wk "Test")
   "tt" '(nil :wk "Function")))

(provide 'lang-dockerfile)
