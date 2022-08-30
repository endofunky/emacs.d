;;; lang-asm.el --- Assembly configuration -*- lexical-binding: t; -*-
(require 'core-lib)

(use-package nasm-mode
  :mode ("\\.asm\\'"))

(use-package x86-lookup
  :general
  (:keymaps 'asm-mode-map :states 'normal
   "K" 'x86-lookup)
  (:keymaps 'nasm-mode-map :states 'normal
   "K" 'x86-lookup)
  :commands (x86-lookup)
  :custom
  (x86-lookup-pdf (+join-file-names
                   user-emacs-directory
                   "etc"
                   "x86-lookup"
                   "325383-sdm-vol-2abcd.pdf")))

(provide 'lang-asm)
