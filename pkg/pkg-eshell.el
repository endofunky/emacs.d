(use-package eshell
  :commands (eshell eshell-command)
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
  (eshell-visual-commands '("el" "elinks" "elm" "htop" "less" "less" "lynx"
                            "more" "ncftp" "pine" "screen" "ssh" "tig" "tin"
                            "tmux" "top" "top" "trn" "vi" "vim"))
  (eshell-visual-options '(("git" "--help" "--paginate")))
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
  (require 'em-term)
  (require 'em-unix)

  (ef-add-hook eshell-mode-hook
    (visual-line-mode t)
    (goto-address-mode t)
    (company-mode -1)
    (setq-local global-hl-line-mode nil)
    (eshell/export "TERM=xterm-256color")
    (when (executable-find "cat")
      (eshell/export "GIT_PAGER=cat")
      (eshell/export "PAGER=cat"))
    (evil-define-key 'normal eshell-mode-map (kbd "C-p") 'eshell-previous-prompt)
    (evil-define-key 'normal eshell-mode-map (kbd "C-n") 'eshell-next-prompt)
    (evil-define-key 'insert eshell-mode-map (kbd "C-p") 'eshell-previous-matching-input-from-input)
    (evil-define-key 'insert eshell-mode-map (kbd "C-n") 'eshell-next-matching-input-from-input)
    (evil-define-key 'insert eshell-mode-map (kbd "TAB") 'pcomplete)
    (evil-define-key 'insert eshell-mode-map (kbd "C-r") 'eshell-insert-history))

  (defun eshell-insert-history ()
    "Displays the eshell history to select and insert back into your eshell."
    (interactive)
    (insert (ivy-completing-read "Eshell history: "
                                 (delete-dups
                                  (ring-elements eshell-history-ring)))))

  (defun ef-eshell-prompt-vc-info ()
    (ignore-errors
      (when-let* ((path (projectile-project-root))
                  (backend (vc-responsible-backend path)))
        (vc-file-clearprops path)
        (let* ((ms (vc-call-backend backend 'mode-line-string path))
               (mss (split-string ms "[\\-\\:\\@\\!\\?]"))
               (vc (downcase (car mss)))
               (branch (car (cdr mss))))
          (set-text-properties 0 (length branch) nil branch)
          (concat
           "("
           (propertize vc 'face 'font-lock-keyword-face)
           "|"
           (propertize branch 'face 'font-lock-keyword-face)
           ")")))))

  (defun ef-eshell-prompt-sign ()
    (let ((sym (if (= (user-uid) 0) "#" "λ")))
      (concat " " (propertize sym 'face `(:foreground "white")) " ")))

  (defun ef-eshell-prompt-path ()
    (let ((path (shrink-path-prompt default-directory)))
      (concat (propertize (car path)
                          'face 'font-lock-comment-face)
              (propertize (cdr path)
                          'face 'font-lock-builtin-face))))

  (defun ef-eshell-prompt ()
    (concat (ef-eshell-prompt-path)
            (ef-eshell-prompt-vc-info)
            (ef-eshell-prompt-sign)))

  ;; Prefer system versions, if available
  (dolist (cmd '("rm" "cp" "mv" "ln" "mkdir" "rmdir"))
    (if (executable-find cmd)
        (fmakunbound (intern (concat "eshell/" cmd)))))

  (defalias 'eshell/ff 'find-file)

  (defun eshell/browse (file)
    "Open FILE in browser"
    (browse-url-of-file file))

  (defun eshell/clear (&rest args)
    "Clear console"
    (eshell/clear-scrollback))

  (defun eshell/cdg (&rest args)
    "Change directory to VC root"
    (let* ((path default-directory)
           (backend (vc-responsible-backend path)))
      (eshell/cd (vc-call-backend backend 'root path))))

  (defun eshell/cdp (&rest args)
    "Change directory to projectile-project-root"
    (eshell/cd (or (projectile-project-root) ".")))

  (defun eshell/d (&rest args)
    "Open directory in dired"
    (dired (or (car args) ".")))

  (if (executable-find "find")
      (defun eshell/find (&rest args)
        "EShell wrapper around the ‘find’ executable."
        (let ((cmd (concat "find " (string-join args " "))))
          (shell-command-to-string cmd))))

  (defun eshell/j (&rest args)
    "Jump to directory of the previous buffer file"
    (when-let ((file (buffer-file-name (other-buffer (current-buffer) 1))))
      (eshell/cd (file-name-directory file))))

  (defun eshell/jg (&rest args)
    "Jump to VC root of the previous buffer file"
    (when-let* ((file (buffer-file-name (other-buffer (current-buffer) 1)))
                (path (file-name-directory file))
                (backend (vc-responsible-backend path)))
      (eshell/cd (vc-call-backend backend 'root path))))

  (defun eshell/jp (&rest args)
    "Jump to VC proctile-project-root of the previous buffer file"
    (when-let ((file (buffer-file-name (other-buffer (current-buffer) 1))))
      (eshell/cd (projectile-project-root file))))

  (defun eshell/q ()
    "Bury current eshell buffer"
    (bury-buffer))

  (defun eshell/sff (file)
    "Use tramp to open FILE using sudo"
    (find-file (format "/sudo:root@localhost:%s" (expand-file-name file))))

  (defun eshell/magit (&rest args)
    "Open magit"
    (magit)
    (eshell/echo)))

(use-package shrink-path
  :after eshell
  :ensure t
  :demand t)

(use-package em-banner
  :after eshell
  :custom
  (eshell-banner-message
   '(if (executable-find "fortune")
        (concat (shell-command-to-string "fortune -s") "\n")
      (concat "Welcome to the Emacs shell " user-login-name "\n\n"))))

(use-package bash-completion
  :ensure t
  :if (executable-find "bash")
  :commands bash-completion-dynamic-complete
  :init
  (add-hook 'shell-dynamic-complete-functions #'bash-completion-dynamic-complete))

(use-package fish-completion
  :ensure t
  :if (executable-find "fish")
  :commands  fish-completion-mode
  :custom
  (fish-completion-fallback-on-bash-p (executable-find "bash"))
  :init
  (add-hook 'eshell-mode-hook #'fish-completion-mode))

(provide 'pkg-eshell)
