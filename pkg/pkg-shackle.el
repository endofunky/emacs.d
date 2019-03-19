(use-package shackle
  :ensure t
  :custom
  (shackle-select-reused-windows nil)
  (shackle-default-alignment 'below)
  (shackle-default-size 0.4)
  (shackle-default-rule '(:same t))
  (shackle-rules '())
  :config
  (defun ef-shackle (shackle &rest shackles)
    "Adds one or more shackle rules to `shackle-rules'"
    (dolist (rule (cons shackle shackles))
      (add-to-list 'shackle-rules rule)))

  (ef-shackle '(" *undo-tree*" :align below :size .4 :popup t :select t)
              '("*company-documentation*" :align below :size .4 :popup t :noselect t)
              '("*compilation*" :align below :size .4 :popup t :noselect t)
              '("*Checkdoc Status*" :align below :size .3 :popup t :no-select t)
              '("*Completions*" :align below :size .3 :popup t :no-select t)
              '("*Help*" :align below :size .4 :popup t :select t))

  (shackle-mode t))

(defmacro ef-define-repl (name buf fn)
  "Define a shackle REPL wrapper function.

Define a new function `NAME' for buffer `BUF' created by calling function `FN'
When `shackle-rules' is bound, will add a popup & select rule for the given
buffer."
  `(progn
     (when (boundp 'shackle-rules)
       (add-to-list 'shackle-rules
                    '(,buf :align bottom :size .4 :popup t :select t)))
     (defun ,name ()
       (interactive)
       (let ((buffer (get-buffer ,buf))
             (window (get-buffer-window ,buf)))
         (cond ((null buffer)
                (funcall ,fn)
                (when (fboundp 'evil-change-state)
                  (evil-change-state 'normal)))
               (window
                (if (one-window-p)
                    (switch-to-prev-buffer nil t)
                  (delete-window window)))
               (t
                (pop-to-buffer ,buf)
                (when (fboundp 'evil-change-state)
                  (evil-change-state 'normal))))))))

(provide 'pkg-shackle)
