(require 'cl-macs)
(require 'subr-x)

(cl-defmacro ef-add-hook(hooks &rest body
                               &key fn append local interactive
                               &allow-other-keys)
  "Define a new handler for HOOKS
By default generates a function named `ef-some-hook' if the first hook in
HOOKS is `some-hook'. Usage:

  (ef-add-hook hook-name/(hook-name...)
     [:keyword [option]]...
     [body]...)

:fn              Override identifier of generated defun.
:append          Append to hook list if non-nil.
:local           Modify the hook's buffer-local value.
:interactive     Mark generated defun as interactive if non-nil. "
  (declare (indent defun))
  (let* ((hooks (if (listp hooks)
                    hooks
                  (list hooks)))
         (hook-name (symbol-name (car hooks)))
         (fn (or fn (intern (concat "ef-" hook-name)))))
    (cl-remf body :fn)
    (cl-remf body :append)
    (cl-remf body :local)
    (cl-remf body :interactive)
    `(progn
       (defun ,fn ()
         ,@(remove nil `(,(concat "Custom handler for " hook-name)
                         ,(if interactive `(interactive))
                         ,@body)))
       ,@(mapcar #'(lambda (hook)
                     `(add-hook ',hook #',fn ,append ,local))
                 hooks))))

(defmacro ef-keep-other-windows (fn)
  "Temporarily disable delete-other-windows for FN."
  `(defadvice ,fn (around ef-keep-other-windows activate)
     (cl-letf (((symbol-function 'delete-other-windows)
                (symbol-function 'ignore)))
       ad-do-it)))

(defmacro ef-wrap-shell-command (cmd)
  "Wrap CMD in an interactive lambda."
  `(lambda ()
     (interactive)
     (start-process-shell-command ,cmd nil ,cmd)))

(defun ef-nsp ()
  "Return t if running on macOS or NeXTSTEP"
  (memq window-system '(mac ns)))

(defun ef-read-file (filename)
  "Return the contents of FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(provide 'core-lib)
