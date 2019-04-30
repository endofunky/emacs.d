(require 'pkg-shackle)

(use-package eshell
  :commands eshell
  :init
  (defun ef-eshell-hud ()
    "Create a eshell HUD."
    (interactive)
    (require 'eshell)
    (let ((prev eshell-buffer-name))
      (setq eshell-buffer-name "*eshell-hud*")
      (eshell)
      (evil-insert 0)
      (setq eshell-buffer-name prev)))

  (ef-shackle '("*eshell-hud*" :align below :size .4 :popup t :select t))

  (global-set-key (quote [f1]) 'ef-eshell-hud)

  (defun eshell-insert-history ()
    "Displays the eshell history to select and insert back into your eshell."
    (interactive)
    (insert (ido-completing-read "Eshell history: "
                                 (delete-dups
                                  (ring-elements eshell-history-ring)))))

  (ef-add-hook eshell-mode-hook
    (evil-define-key 'insert eshell-mode-map (kbd "C-r") 'eshell-insert-history)
    (local-set-key (quote [f1]) 'quit-window))

  (defun eshell/magit (&rest args)
    (magit-status (projectile-project-root))
    (eshell/echo))

  (defun eshell/find (&rest args)
    "EShell wrapper around the ‘find’ executable."
    (let ((cmd (concat "find " (string-join args " "))))
      (shell-command-to-string cmd))))

(provide 'pkg-eshell)
