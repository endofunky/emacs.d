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

(provide 'lang-web)
