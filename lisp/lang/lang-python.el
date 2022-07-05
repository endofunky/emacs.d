;;; lang-python.el --- Python configuration -*- lexical-binding: t; -*-
(require 'core-evil)
(require 'core-eglot)
(require 'core-popup)
(require 'core-tree-sitter)

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
  :config
  (poe-popup "*ipython*")

  (+enable-lsp python-mode)
  (+enable-tree-sitter python-mode)

  (evil-define-key 'normal inferior-python-mode-map ",r" 'run-python)
  (evil-define-key 'normal python-mode-map ",eb" 'python-shell-send-buffer)
  (evil-define-key 'normal python-mode-map ",ed" 'python-shell-send-defun)
  (evil-define-key 'normal python-mode-map ",ef" 'python-shell-send-file)
  (evil-define-key 'visual python-mode-map ",er" 'python-shell-send-region))

(provide 'lang-python)
