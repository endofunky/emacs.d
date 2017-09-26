(defmacro ts/define-repl (name buf fn)
  "Define a popwin REPL wrapper function"
  `(defun ,name ()
     (interactive)
     (let ((buffer-instance (get-buffer ,buf))
           (options '(,buf
                      :position bottom
                      :height .4
                      :stick t
                      :tail t
                      :noselect nil
                      :dedicated t))
           (buf (current-buffer)))
       (cond ((null buffer-instance)
              (funcall ,fn)
              (switch-to-buffer buf)
              (apply 'popwin:popup-buffer options))
             ((get-buffer-window ,buf t)
              (popwin:close-popup-window t))
             (t (apply 'popwin:popup-buffer options))))))

(provide 'core-lib)
