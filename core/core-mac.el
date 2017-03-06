;; Sensible setup for Macs
(when (memq window-system '(mac ns))
  ;; Load PATH from shell unless we inherited an environment.
  (when (not (getenv "TERM_PROGRAM"))
    (use-package exec-path-from-shell
      :ensure t
      :config
      (setq exec-path-from-shell-check-startup-files nil)
      (exec-path-from-shell-initialize)))

  ;; Make dired work properly
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil)

  ;; Key configuration
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  (setq ns-function-modifier 'hyper)

  ;; Load new files in the same frame
  (setq ns-pop-up-frames nil)

  ;; Fix the hash key
  (fset 'insertPound "#")
  (define-key global-map (kbd "s-3") 'insertPound)

  ;; Fix delete-by-moving-to-trash
  (use-package osx-trash
    :ensure t
    :init (osx-trash-setup))

  ;; Enable emoji
  (if (fboundp 'set-fontset-font)
      (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

  (setq default-input-method "MacOSX"))

(provide 'core-mac)
