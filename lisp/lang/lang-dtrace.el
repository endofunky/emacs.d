;;; lang-dtrace.el --- DTrace script configuration -*- lexical-binding: t; -*-
(require 'core-lib)

(use-package dtrace-script-mode
  :mode (("\\.d\\'" . dtrace-script-mode)))

(provide 'lang-dtrace)
