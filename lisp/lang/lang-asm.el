;;; lang-asm.el --- Assembly configuration -*- lexical-binding: t; -*-
(require 'core-lib)

(use-package asm-mode
  :general
  (:keymaps 'asm-mode-map :states 'insert
   ";" 'self-insert-command)
  (:prefix ef-local-leader :states 'normal :keymaps 'asm-mode-map
   "c"  '(nil :wk "Compile")
   "cc" '(compile :wk "Compile")))

(use-package nasm-mode
  :mode ("\\.asm\\'")
  :general
  (:prefix ef-local-leader :states 'normal :keymaps 'nasm-mode-map
   "c"  '(nil :wk "Compile")
   "cc" '(compile :wk "Compie"))
  (:keymaps 'nasm-mode-map :states 'insert
   ";" 'self-insert-command)

  (+add-hook nasm-mode-hook
    (unless (or (file-exists-p "makefile")
		(file-exists-p "Makefile"))
      (setq-local compile-command
                  (concat "nasm -f bin "
                          (file-name-nondirectory buffer-file-name))))))

(use-package flymake-nasm
  :after nasm-mode
  :custom
  (flymake-nasm-format "bin")
  :hook
  (nasm-mode . flymake-nasm-setup))

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
