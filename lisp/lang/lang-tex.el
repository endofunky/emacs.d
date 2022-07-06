;;; lang-tex.el --- TeX/LaTeX configuration -*- lexical-binding: t; -*-
(require 'core-lib)
(require 'core-eglot)
(require 'core-popup)

(use-package auctex
  :defer t
  :custom
  ;; Use normal font height for sections.
  (font-latex-fontify-sectioning 1.0)
  ;; Parse on load.
  (TeX-parse-self t)
  ;; Parse on save.
  (TeX-auto-save t)
  ;; PDF-mode.
  (TeX-source-correlate-method 'synctex)
  ;; Don't start the Emacs server when correlating sources.
  (TeX-source-correlate-start-server nil)
  ;; Automatically insert braces after sub/superscript in `LaTeX-math-mode'.
  (TeX-electric-sub-and-superscript t)
  ;; Don't confirm save before compilation.
  (TeX-save-query nil)
  ;; Do not prompt for a master file.
  (TeX-master t)
  :functions (texmathp)
  :config
  (+enable-lsp tex-mode latex-mode LaTeX-mode plain-tex-mode)
  (+add-hook TeX-mode-hook
    ;; Tell Emacs how to parse TeX files.
    (setq-local ispell-parser 'tex)
    ;; Don't auto-fill in math blocks.
    (setq-local fill-nobreak-predicate
                (cons #'texmathp fill-nobreak-predicate))))

(provide 'lang-tex)
