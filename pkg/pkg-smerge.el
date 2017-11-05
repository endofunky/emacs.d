(use-package smerge-mode
  :ensure t
  :commands smerge-mode
  :init
  (setq smerge-command-prefix (kbd "C-s"))

  (defun ef-enable-smerge-maybe ()
    "Auto-enable `smerge-mode' when merge conflict is detected."
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil :noerror)
        (smerge-mode t))))

  (add-hook 'find-file-hook 'ef-enable-smerge-maybe t)
  (add-hook 'after-revert-hook 'sm-try-smerge t))

(provide 'pkg-smerge)
