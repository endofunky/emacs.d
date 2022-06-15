(require 'core-lib)

(use-package all-the-icons
  :functions (all-the-icons-octicon)
  :config
  ;; Make sure the icon fonts are good to go
  (set-fontset-font t 'unicode (font-spec :family "all-the-icons") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "file-icons") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "Material Icons") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "github-octicons") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "FontAwesome") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "Weather Icons") nil 'append))

(use-package ef-modeline
  :demand t
  :straight nil
  :load-path "vendor/"
  :commands (ef-modeline-mode)
  :config
  (ef-modeline-mode t))

(require 'core-lib)

