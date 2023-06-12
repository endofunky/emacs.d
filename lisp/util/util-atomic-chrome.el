;;; util-atomic-chrome.el --- atomic-chrome configuration -*- lexical-binding: t; -*-
(require 'core-lib)

(use-package atomic-chrome
  :custom
  (atomic-chrome-default-major-mode 'text-mode)
  (atomic-chrome-buffer-open-style 'full)
  (atomic-chrome-url-major-mode-alist '(("github\\.com" . gfm-mode)
                                        ("gist\\.github\\.com" . gfm-mode)
                                        ("localhost:8888" . python-mode)
                                        ("127\\.0\\.0\\.1:8888" . python-mode)))
  :commands
  (atomic-chrome-start-server)
  :config
  (atomic-chrome-start-server))

(provide 'util-atomic-chrome)
