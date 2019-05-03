(require 'pkg-shackle)

(use-package eshell
  :commands eshell
  :custom
  (eshell-buffer-shorthand t)
  (eshell-error-if-no-glob t)
  (eshell-glob-case-insensitive t)
  (eshell-hist-ignoredups t)
  (eshell-history-size 2000)
  (eshell-kill-processes-on-exit t)
  (eshell-prompt-function #'ef-eshell-prompt)
  (eshell-prompt-regexp "^[^#$λ\n]* [#$λ] ")
  (eshell-scroll-to-bottom-on-input 'all)
  (eshell-scroll-to-bottom-on-output 'all)
  :init
  (defmacro ef-defshell (bind name)
    (let* ((name (symbol-name name))
           (buf (concat "*" name "*"))
           (fn (intern (concat "ef-" name))))
      `(progn
         (defun ,fn ()
           (interactive)
           (require 'eshell)
           (let* ((bufn ,buf)
                  (buf (get-buffer bufn)))
             (if (eq (current-buffer) buf)
                 (bury-buffer)
               (let ((prev eshell-buffer-name))
                 (setq eshell-buffer-name bufn)
                 (eshell)
                 (setenv "TERM" "xterm-256color")
                 (setenv "PAGER" "cat")
                 (setq eshell-buffer-name prev)
                 (evil-insert 0)))))

         (global-set-key ,bind #',fn))))

  (ef-defshell (kbd "M-1") eshell-1)
  (ef-defshell (kbd "M-2") eshell-2)
  (ef-defshell (kbd "M-3") eshell-3)
  (ef-defshell (kbd "M-4") eshell-4)
  :config
  ;; Require these early so we can override commands
  (require 'esh-mode)
  (require 'em-unix)

  (ef-add-hook eshell-mode-hook
    (visual-line-mode t)
    (setq-local global-hl-line-mode nil)
    (evil-define-key 'insert eshell-mode-map (kbd "TAB") 'pcomplete)
    (evil-define-key 'insert eshell-mode-map (kbd "C-r") 'eshell-insert-history))

  (defun eshell-insert-history ()
    "Displays the eshell history to select and insert back into your eshell."
    (interactive)
    (insert (ido-completing-read "Eshell history: "
                                 (delete-dups
                                  (ring-elements eshell-history-ring)))))

  (defun ef-eshell-prompt-vc-info ()
    (when-let* ((path (projectile-project-root))
                (backend (vc-responsible-backend path)))
      (vc-file-clearprops path)
      (let ((branch (vc-call-backend backend 'mode-line-string path)))
        (concat "(" (propertize  (format "%s" branch) 'face `(:foreground "magenta")) ")"))))

  (defun ef-eshell-prompt-sign ()
    (let ((sym (if (= (user-uid) 0) "#" "λ")))
      (concat " " (propertize sym 'face `(:foreground "white")) " ")))

  (defun ef-eshell-prompt ()
    (concat (eshell/basename (abbreviate-file-name (eshell/pwd)))
            (ef-eshell-prompt-vc-info)
            (ef-eshell-prompt-sign)))

  ;; Prefer system versions, if available
  (dolist (cmd '("rm" "cp" "mv" "ln" "mkdir" "rmdir"))
    (if (executable-find cmd)
        (fmakunbound (intern (concat "eshell/" cmd)))))

  (defalias 'eshell/ff 'find-file)

  (defun eshell/clear (&rest args)
    (interactive)
    (eshell/clear-scrollback))

  (defun eshell/cdg (&rest args)
    (let* ((path default-directory)
           (backend (vc-responsible-backend path)))
      (eshell/cd (vc-call-backend backend 'root path))))

  (defun eshell/cdp (&rest args)
    (eshell/cd (or (projectile-project-root) ".")))

  (defun eshell/d (&rest args)
    (dired (or (car args) ".")))

  (if (executable-find "find")
      (defun eshell/find (&rest args)
        "EShell wrapper around the ‘find’ executable."
        (let ((cmd (concat "find " (string-join args " "))))
          (shell-command-to-string cmd))))

  (defun eshell/j (&rest args)
    (let ((file (buffer-file-name (other-buffer (current-buffer) 1))))
      (if file
          (eshell/cd (file-name-directory file))
        (eshell/echo "No jump target for previous buffer"))))

  (defun eshell/jp (&rest args)
    (let ((file (buffer-file-name (other-buffer (current-buffer) 1))))
      (if file
          (eshell/cd (projectile-project-root file))
        (eshell/echo "No jump target for previous buffer"))))

  (defun eshell/q ()
    (bury-buffer))

  (defun eshell/magit (&rest args)
    (magit-status (projectile-project-root))
    (eshell/echo)))

(use-package em-banner
  :after eshell
  :custom
  (eshell-banner-message
   '(if (executable-find "fortune")
        (concat (shell-command-to-string "fortune -s") "\n")
      (concat "Welcome to the Emacs shell " user-login-name "\n\n"))))

(provide 'pkg-eshell)
