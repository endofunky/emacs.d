(defun ef-nsp ()
  "Return t if running on macOS or NeXTSTEP"
  (memq window-system '(mac ns)))

(use-package frame
  :if window-system
  :custom
  (frame-title-format '(multiple-frames "%b" "%b"))
  (frame-resize-pixelwise t)
  (blink-cursor-blinks 0)
  :config
  (require 'subr-x)

  (defun ef-read-file (filename)
    "Return the contents of FILENAME."
    (with-temp-buffer
      (insert-file-contents filename)
      (buffer-string)))

  (let ((file (expand-file-name "~/.emacs-font-size")))
    (if (file-exists-p file)
        (set-frame-font (format "DejaVu Sans Mono-%s"
                                (string-trim (ef-read-file file))))
      (set-frame-font "DejaVu Sans Mono-12")))

  (blink-cursor-mode -1)
  (global-set-key (kbd "M-RET") 'toggle-frame-fullscreen))

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
  :if (and (ef-nsp)
           (null (getenv "TERM_PROGRAM")))
  :custom
  (exec-path-from-shell-check-startup-files nil)
  :config
  (customize-set-variable 'exec-path-from-shell-variables
                          (append exec-path-from-shell-variables
                                  '("NIX_PATH"
                                    "NIX_PROFILES"
                                    "NIX_REMOTE"
                                    "NIX_SSL_CERT_FILE"
                                    "NIX_USER_PROFILE_DIR")))
  (message "exec init")
  (exec-path-from-shell-initialize))

(provide 'core-gui)
