(use-package ag
  :defer t
  :ensure t
  :commands (ag ag-project)
  :config
  (setq ag-reuse-buffers t)
  (setq ag-reuse-window t))

(provide 'pkg-ag)
