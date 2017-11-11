(use-package python
  :config
  (defun ef-set-python-shell-interpreter ()
    (if (executable-find "ipython")
        (setq-default python-shell-interpreter "ipython"
                      python-shell-interpreter-args "--simple-prompt -i")
      (setq-default python-shell-interpreter "python"
                    python-shell-interpreter-args "-i")))

  (advice-add 'run-python :before 'ef-set-python-shell-interpreter)

  (ef-define-repl ef-repl-python "*Python*" 'run-python)

  (evil-define-key 'normal python-mode-map ",r" 'ef-repl-python)
  (evil-define-key 'normal inferior-python-mode-map ",r" 'ef-repl-python)

  (evil-define-key 'normal python-mode-map ",eb" 'python-shell-send-buffer)
  (evil-define-key 'normal python-mode-map ",ef" 'python-shell-send-file)
  (evil-define-key 'visual python-mode-map ",er" 'python-shell-send-region)
  (evil-define-key 'normal python-mode-map ",ed" 'python-shell-send-defun))

(use-package anaconda-mode
  :ensure t
  :defer t
  :diminish anaconda-mode
  :commands (anaconda-mode anaconda-eldoc-mode)
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  :config
  (evil-define-key 'normal anaconda-mode-map ",," 'anaconda-mode-find-definitions)
  (evil-define-key 'normal anaconda-mode-map ",." 'anaconda-mode-go-back)
  (ef-shackle '("*Anaconda*" :align bottom :size .4 :popup t :no-select t)))

(use-package company-anaconda
  :after anaconda-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'company-backends 'company-anaconda))

(use-package pyvenv
  :after python
  :ensure t
  :commands (pyvenv-mode)
  :defer-install t
  :init
  (add-hook 'python-mode-hook 'pyvenv-mode))

(provide 'lang-python)
