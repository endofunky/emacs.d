;;; core-shackle.el --- Popup management -*- lexical-binding: t; -*-
(require 'core-lib)
(require 'cl-extra)
(require 'cl-macs)
(require 'cl-seq)
(require 'subr-x)

(defun +shackle (_shackle &rest _shackles))

(defun +add-popup (_mode &rest _rules))

(use-package poe
  :demand t
  :straight nil
  :load-path "site-lisp/"
  :commands (poe-mode)
  :config
  (poe-mode t))

(provide 'core-shackle)
