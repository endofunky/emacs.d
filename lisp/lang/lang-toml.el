;;; lang-toml.el --- TOML configuration -*- lexical-binding: t; -*-
(require 'core-lib)

(use-package toml-mode
  :mode ("\\.tml\\'" "\\.toml\\'"))

(provide 'lang-toml)
