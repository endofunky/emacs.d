;;; lang-python.el --- Python configuration -*- lexical-binding: t; -*-
(require 'core-evil)
(require 'core-eglot)
(require 'core-popup)

(use-package python
  :mode (("\\.py\\'" . python-mode)
         ("\\.wsgi$" . python-mode)
         ("SConstruct\\'" . python-mode)
         ("SConscript\\'" . python-mode))
  :interpreter ("python" . python-mode)
  :custom
  (python-shell-interpreter "ipython")
  (python-shell-buffer-name "ipython")
  (python-shell-interpreter-args "--simple-prompt -i")
  (python-indent-guess-indent-offset-verbose nil)
  :general
  (:prefix ef-local-leader :states 'normal :keymaps 'python-mode-map
   "e"  '(nil :wk "Eval")
   "eb" '(python-shell-send-buffer :wk "Buffer")
   "ed" '(python-shell-send-defun :wk "Defun")
   "ef" '(python-shell-send-file :wk "File")
   "er" '(python-shell-send-region :wk "Region")

   "r"  '(nil :wk "REPL")
   "rr" '(run-python :wk "Open"))
  (:prefix ef-local-leader :states 'visual :keymaps 'python-mode-map
   "e"  '(nil :wk "Eval")
   "er" '(python-shell-send-region :wk "Region"))
  :config
  (poe-popup "*ipython*")

  (+enable-lsp python-mode))

(provide 'lang-python)
