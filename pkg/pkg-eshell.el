(use-package eshell
  :commands (eshell eshell-command)
  :init
  (evil-define-key 'normal global-map ",!" 'eshell))

(use-package em-term
  :defer t
  :config
  (add-to-list 'eshell-visual-commands "ssh")
  (add-to-list 'eshell-visual-commands "htop")
  (add-to-list 'eshell-visual-commands "tail"))

(use-package em-ls
  :defer t
  :config
  (setq eshell-ls-use-colors t))

(use-package esh-mode
  :defer t
  :config
  (defun ef-eshell-ido-history ()
    (interactive)
    (insert
     (ido-completing-read "Eshell history: "
                          (delete-dups
                           (ring-elements eshell-history-ring))
                          nil
                          nil
                          (eshell-get-old-input))))

  (evil-define-key 'normal eshell-mode-map (kbd "C-r") 'ef-eshell-ido-history)
  (evil-define-key 'insert eshell-mode-map (kbd "C-r") 'ef-eshell-ido-history)

  (defun ef-eshell-mode-hook ()
    (setq-local global-hl-line-mode nil)
    (setq truncate-lines nil)
    (set (make-local-variable 'truncate-partial-width-windows) nil)
    (when (fboundp 'xterm-color-filter)
      (setenv "TERM" "xterm-256color")
      (make-local-variable 'xterm-color-preserve-properties)
      (setq xterm-color-preserve-properties t)))

  (add-hook 'eshell-mode-hook 'ef-eshell-mode-hook)
  (evil-define-key 'normal eshell-mode-map "{" 'eshell-previous-prompt)
  (evil-define-key 'normal eshell-mode-map "}" 'eshell-next-prompt)

  (when (fboundp 'xterm-color-filter)
    (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
    (setq eshell-output-filter-functions
          (remove 'eshell-handle-ansi-color eshell-output-filter-functions))))

(use-package em-prompt
  :defer t
  :config
  ;; This is supposedly included in Emacs 26, but it still seems broken.
  (defun eshell-next-prompt (n)
    "Move to end of Nth next prompt in the buffer.
See `eshell-prompt-regexp'."
    (interactive "p")
    (re-search-forward eshell-prompt-regexp nil t n)
    (when eshell-highlight-prompt
      (while (not (get-text-property (line-beginning-position) 'read-only) )
        (re-search-forward eshell-prompt-regexp nil t n)))
    (eshell-skip-prompt))

  (defun eshell-previous-prompt (n)
    "Move to end of Nth previous prompt in the buffer.
See `eshell-prompt-regexp'."
    (interactive "p")
    (backward-char)
    (eshell-next-prompt (- n)))

  (defun ef-protect-eshell-prompt ()
    "Protect Eshell's prompt like Comint's prompts.

E.g. `evil-change-whole-line' won't wipe the prompt. This
is achieved by adding the relevant text properties."
    (let ((inhibit-field-text-motion t))
      (add-text-properties
       (point-at-bol)
       (point)
       '(rear-nonsticky t
                        inhibit-line-move-field-capture t
                        field output
                        read-only t
                        front-sticky (field inhibit-line-move-field-capture)))))

  (add-hook 'eshell-after-prompt-hook 'ef-protect-eshell-prompt))

(use-package eshell-prompt-extras
  :ensure t
  :after eshell
  :init
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  :config
  (setq eshell-highlight-prompt nil
        epe-path-style 'single
        epe-git-untracked-char "…"
        epe-git-dirty-char "●"
        eshell-prompt-function 'epe-theme-lambda))

(defun eshell/tig (&rest args)
  "Open magit-log-all at root."
  (magit-log-all (pop args) nil)
  (eshell/echo))

(defun eshell/gs (&rest args)
  "Open magit-status at root."
  (magit-status (pop args) nil)
  (eshell/echo))

(defun eshell/cdg ()
  "Change directory to the project's root."
  (eshell/cd (locate-dominating-file default-directory ".git")))

(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defun eshell/xe (file)
  "Open file via find-file."
  (find-file file))

(provide 'pkg-eshell)
