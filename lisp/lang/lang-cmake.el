;;; lang-cmake.el --- CMake configuration -*- lexical-binding: t; -*-
(require 'core-lib)

(use-package cmake-mode
  :commands cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

(provide 'lang-cmake)
