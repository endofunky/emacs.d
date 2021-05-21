(require 'core-evil)
(require 'core-shackle)

(use-package python
  :ensure t
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
  (add-hook 'python-mode-hook #'lsp)
  (ef-add-popup "*ipython*")

  (evil-define-key 'normal inferior-python-mode-map ",r" 'run-python)
  (evil-define-key 'normal python-mode-map ",eb" 'python-shell-send-buffer)
  (evil-define-key 'normal python-mode-map ",ed" 'python-shell-send-defun)
  (evil-define-key 'normal python-mode-map ",ef" 'python-shell-send-file)
  (evil-define-key 'visual python-mode-map ",er" 'python-shell-send-region))

(provide 'lang-python)
