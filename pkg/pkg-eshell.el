(require 'pkg-shackle)

(use-package eshell
  :commands eshell
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

  (defun eshell-insert-history ()
    "Displays the eshell history to select and insert back into your eshell."
    (interactive)
    (insert (ido-completing-read "Eshell history: "
                                 (delete-dups
                                  (ring-elements eshell-history-ring)))))

  (ef-add-hook eshell-mode-hook
    (evil-define-key 'insert eshell-mode-map (kbd "C-r") 'eshell-insert-history))

  (defun eshell/cdg (&rest args)
    (let* ((path default-directory)
           (backend (vc-responsible-backend path)))
      (eshell/cd (vc-call-backend backend 'root path))))

  (defun eshell/cdp (&rest args)
    (eshell/cd (or (projectile-project-root) ".")))

  (defun eshell/find (&rest args)
    "EShell wrapper around the ‘find’ executable."
    (let ((cmd (concat "find " (string-join args " "))))
      (shell-command-to-string cmd)))

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
