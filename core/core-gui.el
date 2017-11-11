(defun ef-nsp ()
  "Return t if running on macOS or NeXTSTEP"
  (memq window-system '(mac ns)))

(use-package frame
  :if window-system
  :config
  (setq frame-title-format '(multiple-frames "%b" "%b")
        frame-resize-pixelwise t
        blink-cursor-blinks 0)

  (set-frame-font "Inconsolata-11")
  (blink-cursor-mode -1)
  (global-set-key (kbd "M-RET") 'toggle-frame-fullscreen)
  (add-to-list 'initial-frame-alist '(fullscreen . fullboth)))

(use-package ns-win
  :if (ef-nsp)
  :config
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super
        ns-function-modifier 'hyper
        ns-pop-up-frames nil
        ns-use-native-fullscreen nil
        default-input-method "MacOSX")

  (fset 'insertPound "#")
  (define-key global-map (kbd "s-3") 'insertPound)

  (if (fboundp 'set-fontset-font)
      (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)))

(use-package ls-lisp
  :if (ef-nsp)
  :after ns-win
  :config
  (setq-default ls-lisp-use-insert-directory-program nil))

(use-package exec-path-from-shell
  :ensure t
  :after ns-win
  :defer-install t
  :if (and (ef-nsp)
           (null (getenv "TERM_PROGRAM")))
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

(provide 'core-gui)
