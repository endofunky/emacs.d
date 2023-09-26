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
  (:prefix ef-local-leader :states 'normal :keymaps 'python-base-mode-map
   "e"  '(nil :wk "Eval")
   "eb" '(python-shell-send-buffer :wk "Buffer")
   "ed" '(python-shell-send-defun :wk "Defun")
   "ef" '(python-shell-send-file :wk "File")
   "er" '(python-shell-send-region :wk "Region")

   "r"  '(nil :wk "REPL")
   "rr" '(run-python :wk "Open"))
  (:prefix ef-local-leader :states 'visual :keymaps 'python-base-mode-map
   "e"  '(nil :wk "Eval")
   "er" '(python-shell-send-region :wk "Region"))
  :config
  (poe-popup "*ipython*")

  (+enable-lsp python-base-mode))

(use-package python-black
  :if (executable-find "black")
  :after python
  :functions (python-black-on-save-mode
              python-black--third-party-file-p)
  :config
  (+add-hook python-base-mode-hook
    (setq-local fill-column 79)

    (when-let* ((file (buffer-file-name))
                (ok (not (python-black--third-party-file-p file))))
      (python-black-on-save-mode))))

(use-package pytest
  :after python
  :general
  (:prefix ef-local-leader :states 'normal :keymaps 'python-base-mode-map
   "t"  '(nil :wk "Test")
   "tl" '(ruby-test-toggle-implementation-and-specification :wk "Toggle")
   "tp" '(pytest-one :wk "At point")
   "tt" '(pytest-module :wk "Buffer"))
  :config
  (defun pytest-cmd-format (_format-string
                            _working-directory
                            _test-runner
                            _command-flags
                            test-names)
    "Ignore other format string values and override command so pytest works with
direnv."
    (format "pytest -s --no-header %s" test-names)))

(provide 'lang-python)
