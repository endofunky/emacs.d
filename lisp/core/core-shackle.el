;;; core-shackle.el --- Popup management -*- lexical-binding: t; -*-
(require 'core-lib)

(defun +shackle (_shackle &rest _shackles))

(use-package poe
  :demand t
  :straight nil
  :load-path "site-lisp/"
  :commands (poe-mode)
  :general
  (:keymaps 'override
   "M-l" 'poe-popup-next
   "M-h" 'poe-popup-prev
   "M-p" 'poe-popup-toggle
   "M-q" 'poe-popup-kill)
  :config
  (poe-popup " *Metahelp*" :ephemeral t)
  (poe-popup " *undo-tree*" :ephemeral t)
  (poe-popup "*Apropos*" :size .3 :ephemeral t)
  (poe-popup "*Backtrace*")
  (poe-popup "*Checkdoc Status*" :ephemeral t)
  (poe-popup "*Compile-Log*" :ephemeral t)
  (poe-popup "*Command History*")
  (poe-popup "*Help*" :ephemeral t)
  (poe-popup "*Messages*")
  (poe-popup "*Occur*" :ephemeral t)
  (poe-popup "*Pp Eval Output*")
  (poe-popup "*Warnings*" :ephemeral t)
  (poe-popup "*compilation*")
  (poe-popup "\\`\\*WoMan.*?\\*\\'" :regexp t :ephemeral t)
  (poe-popup 'calendar-mode :ephemeral t)
  (poe-popup 'comint-mode)
  (poe-popup 'compilation-mode)
  (poe-mode t))

(provide 'core-shackle)
