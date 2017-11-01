(use-package shackle
  :ensure t
  :config
  (setq shackle-select-reused-windows nil
        shackle-default-alignment 'below
        shackle-default-size 0.4
        shackle-default-rule '(:same t))

  (when (not shackle-rules)
    (setq shackle-rules '()))

  (defun ef-shackle (shackle &rest shackles)
    "Adds one or more shackle rules to `shackle-rules'"
    (dolist (rule (cons shackle shackles))
       (add-to-list 'shackle-rules rule)))

  (ef-shackle '(" *undo-tree*" :align below :size .4 :popup t :select t)
              '("*company-documentation*" :align below :size .4 :popup t :noselect t)
              '("*compilation*" :align below :size .4 :popup t :noselect t)
              '("*Help*" :align below :size .4 :popup t :select t))

  (shackle-mode t)

  (defmacro ef-define-repl (name buf fn)
    "Define a shackle REPL wrapper function with `name' for buffer `buf'
created by calling function `fn'"
    `(progn
       (add-to-list 'shackle-rules
                    '(,buf :align bottom :size .4 :popup t :select t))
       (defun ,name ()
         (interactive)
         (let ((buffer (get-buffer ,buf))
               (window (get-buffer-window ,buf t)))
           (cond ((null buffer)
                  (funcall ,fn))
                 (window
                  (delete-window window))
                 (t
                  (pop-to-buffer ,buf))))))))


(provide 'pkg-shackle)
