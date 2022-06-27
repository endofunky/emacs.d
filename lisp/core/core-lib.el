(require 'use-package)

(defgroup ef nil
  "Endomacs customizations."
  :group 'faces)

(defcustom ef-leader ","
  "Leader key used as prefix in keymaps."
  :group 'ef
  :type 'string)

(defcustom ef-local-leader ","
  "Local leader key used as prefix in major-mode specific keymaps."
  :group 'ef
  :type 'string)

(defcustom ef-fullscreen-indicator "orange red"
  "Set foreground color for fullscreen indicator in mode-line."
  :group 'ef
  :type 'string)

(use-package gcmh
  :demand t
  :commands (gcmh-mode)
  :config
  (gcmh-mode 1))

(use-package which-key
  :demand t
  :commands (which-key-mode)
  :custom
  (which-key-idle-delay 0.5)
  (which-key-idle-secondary-delay 0)
  (which-key-is-verbose t)
  (which-key-separator " ")
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  ;; We have font-lock displaying prefixes in a different color,
  ;; so there is no need for a prefix character.
  (which-key-prefix-prefix "")
  :config
  (which-key-mode t))

(use-package general
  :after which-key
  :commands (general-override-mode)
  :config
  (declare-function general-auto-unbind-keys "ext:general")
  (declare-function general-define-key "ext:general")

  (general-auto-unbind-keys)
  (general-override-mode t)
  (general-define-key
   :states '(normal insert visual motion replace emacs)
   :keymaps 'override
   "M-f" 'ef-toggle-window-fullscreen))

(use-package page-break-lines
  :demand t
  :commands (global-page-break-lines-mode)
  :config
  (global-page-break-lines-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Utilities
;;

(defsubst ef-nsp ()
  "Return t if running on macOS or NeXTSTEP."
  (memq window-system '(mac ns)))

(defsubst ef-apply-reverse (fn &rest args)
  "Call FN with it's arguments ARGS reversed."
  (apply fn (reverse args)))

(defsubst ef-as-list (value-or-list)
  "If VALUE-OR-LIST is already a string, return it.  Otherwise
convert it to a list and return that."
  (if (listp value-or-list)
      value-or-list
    (list value-or-list)))

(defsubst ef-as-string (string-or-symbol)
  "If STRING-OR-SYMBOL is already a string, return it.  Otherwise
convert it to a string and return that."
  (if (stringp string-or-symbol)
      string-or-symbol
    (symbol-name string-or-symbol)))

(defsubst ef-as-symbol (string-or-symbol)
  "If STRING-OR-SYMBOL is already a symbol, return it.  Otherwise
convert it to a symbol and return that."
  (if (symbolp string-or-symbol)
      string-or-symbol
    (intern string-or-symbol)))

(defsubst ef-mode-name (name)
  "If NAME ends in `-mode' (or its name does), return it as a
string.  Otherwise, return it as a string with `-mode' appended."
  (if-let* ((string (ef-as-string name))
            (_ (string-match "-mode\\'" string)))
      string
    (concat string "-mode")))

(defsubst ef-mode (name)
  "If NAME ends in `-mode' (or its name does), return it as a
symbol.  Otherwise, return it as a symbol with `-mode' appended."
  (intern (ef-mode-name name)))

(defsubst ef-mode-map-name (name)
  "If NAME ends in `-mode-map' (or its name does), return it as a
string.  Otherwise, return it as a string with `-mode-map'
appended."
  (if-let* ((string (ef-as-string name))
            (_ (string-match "-map\\'" string)))
      string
    (concat (ef-mode-name string) "-map")))

(defsubst ef-mode-map (name)
  "If NAME ends in `-mode-map' (or its name does), return it as a
symbol.  Otherwise, return it as a symbol with `-mode-map'
appended."
  (intern (ef-mode-map-name name)))

(defsubst ef-mode-hook-name (name)
  "If NAME ends in `-mode-hook' (or its name does), return it as a
string.  Otherwise, return it as a string with `-mode-hook'
appended."
  (if-let* ((string (ef-as-string name))
            (_ (string-match "-hook\\'" string)))
      string
    (concat (ef-mode-name string) "-hook")))

(defsubst ef-mode-hook (name)
  "If NAME ends in `-mode-hook' (or its name does), return it as a
symbol.  Otherwise, return it as a symbol with `-mode-hook'
appended."
  (intern (ef-mode-hook-name name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Property lists
;;

(defun ef-plist-merge (&rest plists)
  "Create a single property list from all plists in PLISTS.
The process starts by copying the first list, and then setting
properties from the other lists.  Settings in the last list
are the most significant ones and overrule settings in the
other lists.

This is coming from `org-mode' (`org-combine-plists'). Requiring
`org-mode' loads a 24k+ line Emacs Lisp file, which introduces
significant overhead when used as a utility library, hence it
has been extracted."
  (let ((rtn (copy-sequence (pop plists)))
        p v ls)
    (while plists
      (setq ls (pop plists))
      (while ls
        (setq p (pop ls) v (pop ls))
        (setq rtn (plist-put rtn p v))))
    rtn))

(defun ef-plist-merge-reverse (&rest plists)
  "Create a single property list from all plists in PLISTS.
The process starts by copying the last list, and then setting
properties from the other lists in reverse order.  Settings in
the first list are the most significant ones and overrule
settings in the other lists."
  (apply #'ef-plist-merge (reverse plists)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Lists
;;

(defun ef-move-to-front (elt list)
  "Add/mode ELT to the front of LIST."
  (cons elt (remove elt list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Configuration macros
;;

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
    (let ((docstr (if (stringp (car body))
		      (prog1
			  (car body)
			(setq body (cdr body)))
		    (concat "Custom handler for " hook-name))))
      `(progn
	 (defun ,fn ()
           ,@(remove nil `(,docstr
                           ,(if interactive `(interactive))
                           ,@body)))
	 ,@(mapcar #'(lambda (hook)
                       `(add-hook ',hook ',fn ,append ,local))
                   hooks)))))

(defmacro ef-customize (&rest cvars)
  "Generate custom-set-variables code for CVARS."
  (declare (indent defun))
  `(let ((custom--inhibit-theme-enable nil))
     (unless (memq 'ef-custom custom-known-themes)
       (deftheme ef-custom)
       (enable-theme 'ef-custom)
       (setq custom-enabled-themes (remq 'ef-custom custom-enabled-themes)))
     (custom-theme-set-variables
      'ef-custom
      ,@(mapcar #'(lambda (def)
                    (let ((feature (or load-file-name (buffer-file-name)))
                          (symbol (car def))
                          (exp (cadr def))
                          (comment (or (caddr def)
                                       "Customized with ef-customize")))
                      `'(,symbol ,exp nil nil ,comment)))
                cvars))))

(defmacro ef-keep-other-windows (fn)
  "Temporarily disable delete-other-windows for FN."
  `(defadvice ,fn (around ef-keep-other-windows activate)
     (cl-letf (((symbol-function 'delete-other-windows)
                (symbol-function 'ignore)))
       ad-do-it)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commands
;;

(defun ef-toggle-window-fullscreen ()
  "Toggle current window fullscreen."
  (interactive)
  (let ((mode-line-str (propertize "FS"
				   'font-lock-face
				   (list :foreground ef-fullscreen-indicator))))
    (if (= 1 (length (window-list)))
        (progn
          (jump-to-register :ef-fullscreen)
          (setq global-mode-string (delete mode-line-str global-mode-string)))
      (window-configuration-to-register :ef-fullscreen)
      (delete-other-windows)
      (setq global-mode-string (push mode-line-str global-mode-string)))))

(defun ef-read-file (filename)
  "Return the contents of FILENAME."
  (interactive "f")
  (if (called-interactively-p 'interactive)
      (insert-file-contents filename)
    (with-temp-buffer
      (insert-file-contents filename)
      (buffer-string))))

(defun ef-insert-unix-time ()
  "Insert UNIX timestamp at point."
  (interactive)
  (insert (format-time-string "%s")))

(defun ef-insert-uuid ()
  "Insert UUID at point (requires uuidgen to be installed)."
  (interactive)
  (if-let (uuidgen (executable-find "uuidgen"))
      (insert (string-trim (shell-command-to-string uuidgen)))
    (error "Binary 'uuidgen' not found in PATH.")))

(defun ef-insert-file-name ()
  "Insert current file name at point."
  (interactive)
  (insert (file-name-nondirectory (buffer-file-name))))

(defun ef-insert-file-name-base ()
  "Insert current file base name at point."
  (interactive)
  (insert (file-name-base (buffer-file-name))))

(defun ef-insert-file-name-directory ()
  "Insert current file name directory at point."
  (interactive)
  (insert (file-name-directory (buffer-file-name))))

(defun ef-insert-iso-datetime ()
  "Insert ISO 8601 date/time at point."
  (interactive)
  (insert (concat
	   (format-time-string "%Y-%m-%dT%T")
	   ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
	    (format-time-string "%z")))))

(defun ef-insert-ordinal-date ()
  "Insert ordinal date at point."
  (interactive)
  (insert (format-time-string "%Y-%j")))

(defun ef-kill-buffer-or-delete-window ()
  "If more than one window is open, delete the current window, otherwise kill
current buffer."
  (interactive)
  (if (> (length (window-list)) 1)
      (delete-window)
    (kill-buffer)))

(defun ef-indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun ef-kill-buffers-matching (filter)
  "Kill all other buffers matching FILTER, except the *scratch* buffer.

If FILTER is `nil' kill all buffers except the current one and the *scratch*
buffer."
  (interactive "sFilter: ")
  (dolist (buf (delq (current-buffer) (buffer-list)))
    (let ((name (buffer-name buf)))
      (when (and (not (string= name "*scratch*"))
                 (or (not filter)
                     (string-match filter (string-trim name))))
        (if-let ((win (get-buffer-window buf)))
            (delete-window win))
        (kill-buffer buf)))))

(defun ef-kill-other-buffers ()
  "Kill all other buffers except special buffers."
  (interactive)
  (ef-kill-buffers-matching "^[^\\*]"))

(defun ef-kill-all-other-buffers ()
  "Kill all other buffers except the *scratch* buffer."
  (interactive)
  (ef-kill-buffers-matching nil))

(provide 'core-lib)
