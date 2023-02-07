;;; lang-web.el --- HTML/etc. -*- lexical-binding: t; -*-
(require 'use-package)

(use-package web-mode
  :custom
  ;; This is causing way too many different colors, which makes things hard
  ;; to read.
  (web-mode-enable-html-entities-fontification nil)
  ;; Auto-close on </.
  (web-mode-auto-close-style 1)
  :mode (("\\.[px]?html?\\'". web-mode)
         ("\\.html\\.erb?\\'". web-mode)))

(use-package haml-mode
  :mode (("\\.haml?\\'". haml-mode)))

(use-package slim-mode
  :mode (("\\.slim?\\'". slim-mode)))

(provide 'lang-web)
