(require 'cl-macs)
(require 'core-evil)
(require 'core-lib)

(use-package fringe
  :if window-system
  :custom
  (fringes-outside-margins nil)
  (indicate-buffer-boundaries nil)
  (indicate-empty-lines nil)
  (overflow-newline-into-fringe t)
  :config
  (ef-add-hook window-configuration-change-hook :fn ef-fringe-update-hook
    (let ((left (frame-parameter nil 'left-fringe))
          (right (frame-parameter nil 'right-fringe)))
      (cl-loop for win being the windows
               if (with-selected-window win (derived-mode-p 'prog-mode))
               do (set-window-fringes win nil nil)
               else do (set-window-fringes win 0 0))))
  (fringe-mode nil))

(use-package frame
  :if window-system
  :custom
  (blink-cursor-blinks 0)
  (frame-resize-pixelwise t)
  (frame-title-format '(multiple-frames "%b" "%b"))
  :general
  ("M-<return>" 'toggle-frame-fullscreen)
  :config
  (let* ((file (expand-file-name "~/.emacs-font-size"))
         (font (if (file-exists-p file)
                   (format "DejaVu Sans Mono-%s" (string-trim (ef-read-file file)))
                 "DejaVu Sans Mono-12")))
    (add-to-list 'default-frame-alist `(font .  ,font))
    (set-face-attribute 'default t :font font)
    (set-frame-font font))

  (defadvice blink-cursor-start (around ef-blink-cursor-start activate)
    "Only blink in comint-mode buffers and only in evil normal state."
    (if (and (or (derived-mode-p 'comint-mode)
                 (derived-mode-p 'cider-repl-mode))
             (eq evil-state 'normal))
        ad-do-it))

  (blink-cursor-mode t))

(use-package ns-win
  :if (ef-nsp)
  :defines (mac-command-modifier
            mac-option-modifier
            ns-function-modifier
            ns-pop-up-frames
            ns-use-native-fullscreen)
  :general
  ("s-3" 'insertPound)
  :config
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super
        ns-function-modifier 'hyper
        ns-pop-up-frames nil
        ns-use-native-fullscreen nil
        default-input-method "MacOSX")

  (fset 'insertPound "#")

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
  (exec-path-from-shell-variables
   (append exec-path-from-shell-variables
           '("NIX_PATH"
             "NIX_PROFILES"
             "NIX_REMOTE"
             "NIX_SSL_CERT_FILE"
             "NIX_USER_PROFILE_DIR")))
  :config
  (exec-path-from-shell-initialize))

(provide 'core-gui)
