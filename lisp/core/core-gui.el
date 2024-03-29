;;; core-gui.el --- GUI configuration -*- lexical-binding: t; -*-
(require 'cl-macs)
(require 'core-evil)
(require 'core-lib)

(use-package fringe
  :if (display-graphic-p)
  :straight nil
  :custom
  (fringes-outside-margins nil)
  (indicate-buffer-boundaries nil)
  (indicate-empty-lines nil)
  (overflow-newline-into-fringe t)
  :config
  (fringe-mode nil))

(use-package x-win
  :if (eq 'x window-system)
  :straight nil
  :custom
  ;; Don't use GTK tooltips
  (x-gtk-use-system-tooltips nil))

(use-package pixel-scroll
  :when (and (display-graphic-p)
             (fboundp 'pixel-scroll-precision-mode))
  :straight nil
  :hook
  (after-init . pixel-scroll-precision-mode))

(use-package frame
  :straight nil
  :if (display-graphic-p)
  :custom
  (frame-resize-pixelwise t)
  :general
  ("M-<return>" 'toggle-frame-fullscreen)
  :config
  (blink-cursor-mode 0)

  (let* ((file (expand-file-name "~/.emacs-font-size"))
         (font (if (file-exists-p file)
                   (format "DejaVu Sans Mono-%s"
                           (string-trim (+read-file file)))
                 "DejaVu Sans Mono-9")))
    (add-to-list 'default-frame-alist `(font .  ,font))
    (set-face-attribute 'default t :font font)
    (set-frame-font font))

  ;; Enable transparent titlebars on macOS and make them match the frame
  ;; background.
  (when (and (eq system-type 'darwin)
             (boundp 'ns-auto-titlebar-set-frame))
    (defun +ns-set-titlebar-frame (frame &rest _)
      "Enable transparent titlebar for the given frame and set the
ns-appearance frame parameter for FRAME to match the
\"background-mode\" frame-parameter."
      (let ((background (frame-parameter frame 'background-mode)))
        (modify-frame-parameters frame `((ns-transparent-titlebar . t)
                                         (ns-appearance . ,background)))))

    (defun +ns-set-titlebar-all-frames (&rest _)
      "Enable transparent titlebar for the given frame and set the
ns-appearance frame parameter for FRAME to match the
\"background-mode\" frame-parameter for all frames."
      (mapc #'ns-auto-titlebar-set-frame (frame-list)))

    (add-hook 'after-init-hook #'+ns-set-titlebar-all-frames)
    (add-hook 'after-make-frame-functions #'+ns-set-titlebar-frame)
    (advice-add 'frame-set-background-mode :after #'+ns-set-titlebar-frame)))

(use-package ns-win
  :straight nil
  :if (+nsp)
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
  :straight nil
  :if (+nsp)
  :after ns-win
  :config
  (setq-default ls-lisp-use-insert-directory-program nil))

(use-package exec-path-from-shell
  :if (and (+nsp)
           (null (getenv "TERM_PROGRAM")))
  :demand (and (+nsp)
               (null (getenv "TERM_PROGRAM")))
  :commands (exec-path-from-shell-initialize)
  :defines (exec-path-from-shell-variables)
  :config
  (exec-path-from-shell-initialize)
  (+csetq
   exec-path-from-shell-check-startup-files nil
   exec-path-from-shell-variables
   (append exec-path-from-shell-variables
           '("NIX_PATH"
             "NIX_PROFILES"
             "NIX_REMOTE"
             "NIX_SSL_CERT_FILE"
             "NIX_USER_PROFILE_DIR"))))

(provide 'core-gui)
