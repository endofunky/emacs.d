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
  (+add-hook window-configuration-change-hook :fn +fringe-update-h
    (let ((left (frame-parameter nil 'left-fringe))
          (right (frame-parameter nil 'right-fringe)))
      (cl-loop for win being the windows
               if (with-selected-window win (derived-mode-p 'prog-mode))
               do (set-window-fringes win nil nil)
               else do (set-window-fringes win 0 0))))
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
  (blink-cursor-blinks 0)
  (frame-resize-pixelwise t)
  (frame-title-format '(multiple-frames "%b" "%b"))
  :general
  ("M-<return>" 'toggle-frame-fullscreen)
  :config
  (let* ((file (expand-file-name "~/.emacs-font-size"))
         (font (if (file-exists-p file)
                   (format "DejaVu Sans Mono-%s"
                           (string-trim (+read-file file)))
                 "DejaVu Sans Mono-11")))
    (add-to-list 'default-frame-alist `(font .  ,font))
    (set-face-attribute 'default t :font font)
    (set-frame-font font))

  (defadvice blink-cursor-start (around ef-blink-cursor-start activate)
    "Only blink in comint-mode when in normal state or in
minibuffers."
    (if (or (and (or (derived-mode-p 'comint-mode)
                     (derived-mode-p 'cider-repl-mode))
                 (eq evil-state 'normal))
            (minibufferp))
        ad-do-it
      (internal-show-cursor nil t)))

  (blink-cursor-mode t)

  ;; Enable transparent titlebars on macOS and make them match the frame
  ;; background.
  (when (eq system-type 'darwin)
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
  :custom
  (exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-variables
   (append exec-path-from-shell-variables
           '("NIX_PATH"
             "NIX_PROFILES"
             "NIX_REMOTE"
             "NIX_SSL_CERT_FILE"
             "NIX_USER_PROFILE_DIR")))
  :commands (exec-path-from-shell-initialize)
  :config
  (exec-path-from-shell-initialize))

(provide 'core-gui)
