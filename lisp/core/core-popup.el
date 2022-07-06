;;; core-popup.el --- Popups -*- lexical-binding: t; -*-
(require 'use-package)

(use-package poe
  :demand t
  :straight nil
  :load-path "site-lisp/"
  :general
  (:keymaps 'override
   "M-l" 'poe-popup-next
   "M-h" 'poe-popup-prev
   "M-p" 'poe-popup-toggle
   "M-q" 'poe-popup-kill)
  :hook (after-init . poe-mode)
  :config
  (poe-popup " *Metahelp*" :ephemeral t)
  (poe-popup "*Apropos*" :size .3 :shrink t :ephemeral t)
  (poe-popup "*Backtrace*")
  (poe-popup "*Checkdoc Status*" :ephemeral t)
  (poe-popup "*Compile-Log*" :ephemeral t)
  (poe-popup "*Command History*")
  (poe-popup "*Help*" :size 0.5 :shrink t :ephemeral t)
  (poe-popup "*Messages*")
  (poe-popup "*Occur*" :ephemeral t)
  (poe-popup "*Pp Eval Output*")
  (poe-popup "*Warnings*" :ephemeral t)
  (poe-popup "*compilation*")
  (poe-popup "\\`\\*WoMan.*?\\*\\'" :regexp t :ephemeral t)
  (poe-popup 'calendar-mode :ephemeral t)
  (poe-popup 'comint-mode)
  (poe-popup 'compilation-mode)

  (poe-rule "*info*" :same t)

  (with-eval-after-load 'consult
    (consult-customize consult--source-buffer :hidden t :default nil)
    (add-to-list 'consult-buffer-sources poe-consult-source))

  ;; Don't have evil's window movement/rotate commands affect popup windows.
  (with-eval-after-load 'evil
    (advice-add 'evil-window-rotate-downwards :around #'poe-popup-save-a)
    (advice-add 'evil-window-rotate-upwards   :around #'poe-popup-save-a)
    (advice-add 'evil-window-move-very-bottom :around #'poe-popup-save-a)
    (advice-add 'evil-window-move-very-top    :around #'poe-popup-save-a)
    (advice-add 'evil-window-move-far-left    :around #'poe-popup-save-a)
    (advice-add 'evil-window-move-far-right   :around #'poe-popup-save-a))

  ;; Some packages set the major mode *after* displaying buffers. This
  ;; disablees all buffer-local minor modes including `poe-popup-mode'. So let's
  ;; re-enable it here (all the window parameters will still be set).
  (defun +re-enable-popup-h ()
    "Re-enable `poe-popup-mode' for `undo-tree-visualize', if required."
    (when (and (bound-and-true-p poe-mode)
               (poe--match (current-buffer)))
      (poe-popup-mode t)))

  (with-eval-after-load 'undo-tree
    (poe-popup " *undo-tree*" :select t :ephemeral t)
    (add-hook 'undo-tree-visualizer-mode-hook #'+re-enable-popup-h))

  (with-eval-after-load 'nix-repl
    (poe-popup "*Nix-REPL*")
    (add-hook 'nix-repl-mode-hook #'+re-enable-popup-h)))

(autoload 'poe-popup "poe")
(autoload 'poe-rule "poe")

(provide 'core-popup)
