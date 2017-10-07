(use-package popwin
  :ensure t
  :config
  (popwin-mode 1)

  (defmacro ts/define-repl (name buf fn)
    "Define a popwin REPL wrapper function"
    `(progn
       (add-to-list 'popwin:special-display-config
                    '(,buf
                      :position bottom
                      :height .4
                      :stick t
                      :tail t
                      :noselect nil
                      :dedicated t))
       (defun ,name ()
         (interactive)
         (let ((buffer-instance (get-buffer ,buf))
               (buf (current-buffer)))
           (cond ((null buffer-instance)
                  (funcall ,fn)
                  (display-buffer buf))
                 ((get-buffer-window ,buf t)
                  (popwin:close-popup-window t)
                  (let ((native-window (get-buffer-window ,buf t)))
                    (when native-window
                      (delete-window native-window))))
                 (t
                  (popwin:pop-to-buffer ,buf)))))))

  (add-to-list 'popwin:special-display-config
               '("*ag search*"
                 :dedicated t
                 :position bottom
                 :stick nil
                 :noselect nil
                 :height 0.4))

  (add-to-list 'popwin:special-display-config
               '("*HTTP Response*"
                 :dedicated t
                 :position bottom
                 :stick nil
                 :noselect nil
                 :height 0.4))

  (add-to-list 'popwin:special-display-config
               '("*Help*"
                 :dedicated t
                 :position bottom
                 :stick t
                 :noselect nil
                 :height 0.3))

  (add-to-list 'popwin:special-display-config
               '("*compilation*"
                 :dedicated t
                 :position bottom
                 :stick t
                 :noselect t
                 :height 0.3))

  (add-to-list 'popwin:special-display-config
               '("*Shell Command Output*"
                 :dedicated t
                 :position bottom
                 :stick t
                 :noselect nil))

  (add-to-list 'popwin:special-display-config
               '("*Async Shell Command*"
                 :dedicated t
                 :position bottom
                 :stick t
                 :noselect nil))

  (add-to-list 'popwin:special-display-config
               '(" *undo-tree*"
                 :dedicated t
                 :position bottom
                 :stick t
                 :noselect nil
                 :height 0.3)))

(provide 'pkg-popwin)
