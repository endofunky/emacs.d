;;; core-lib.el --- Core functions & packages -*- lexical-binding: t; -*-
(require 'straight)
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


;;; Core ext

(use-package general
  :config
  (declare-function general-override-mode "general")
  (declare-function general-auto-unbind-keys "general")
  (declare-function general-define-key "general")

  (general-auto-unbind-keys)
  (general-override-mode t))

(use-package minibuffer-header
  :straight (:type git
             :host github
             :repo "rougier/minibuffer-header")
  :hook (ef-first-command . minibuffer-header-mode))

(use-package which-key
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
  :hook (ef-first-command . which-key-mode))

;; Display form-feed markers (^L) as horizontal lines.
(use-package page-break-lines
  :demand t
  :commands (global-page-break-lines-mode)
  :config
  (global-page-break-lines-mode))


;;; Hooks

(defvar ef-first-command-hook nil
  "Transient hooks run before the first user input.")

(defun +run-first-command-hook-h (&optional _)
  "Runs `ef-first-command-hook' and subsequently resets the currently
registered hooks so they only fire once."
  (run-hooks 'ef-first-command-hook)
  (setq ef-first-command-hook nil))

(add-hook 'pre-command-hook #'+run-first-command-hook-h)


;;; Utilities

(defsubst +nsp ()
  "Return t if running on macOS or NeXTSTEP."
  (memq window-system '(mac ns)))

(defsubst +apply-reverse (fn &rest args)
  "Call FN with it's arguments ARGS reversed."
  (apply fn (reverse args)))

(defun +recenter-a (&rest _)
  "Advice function for recentering window."
  (recenter))

(defsubst +as-list (value-or-list)
  "If VALUE-OR-LIST is already a string, return it.  Otherwise
convert it to a list and return that."
  (if (listp value-or-list)
      value-or-list
    (list value-or-list)))

(defsubst +as-string (string-or-symbol)
  "If STRING-OR-SYMBOL is already a string, return it.  Otherwise
convert it to a string and return that."
  (if (stringp string-or-symbol)
      string-or-symbol
    (symbol-name string-or-symbol)))

(defsubst +as-symbol (string-or-symbol)
  "If STRING-OR-SYMBOL is already a symbol, return it.  Otherwise
convert it to a symbol and return that."
  (if (symbolp string-or-symbol)
      string-or-symbol
    (intern string-or-symbol)))

(defsubst +mode-name (name)
  "If NAME ends in `-mode' (or its name does), return it as a
string.  Otherwise, return it as a string with `-mode' appended."
  (if-let* ((string (+as-string name))
            (ok (string-match "-mode\\'" string)))
      string
    (concat string "-mode")))

(defsubst +mode (name)
  "If NAME ends in `-mode' (or its name does), return it as a
symbol.  Otherwise, return it as a symbol with `-mode' appended."
  (intern (+mode-name name)))

(defsubst +mode-map-name (name)
  "If NAME ends in `-mode-map' (or its name does), return it as a
string.  Otherwise, return it as a string with `-mode-map'
appended."
  (if-let* ((string (+as-string name))
            (ok (string-match "-map\\'" string)))
      string
    (concat (+mode-name string) "-map")))

(defsubst +mode-map (name)
  "If NAME ends in `-mode-map' (or its name does), return it as a
symbol.  Otherwise, return it as a symbol with `-mode-map'
appended."
  (intern (+mode-map-name name)))

(defsubst +mode-hook-name (name)
  "If NAME ends in `-mode-hook' (or its name does), return it as a
string.  Otherwise, return it as a string with `-mode-hook'
appended."
  (if-let* ((string (+as-string name))
            (ok (string-match "-hook\\'" string)))
      string
    (concat (+mode-name string) "-hook")))

(defsubst +mode-hook (name)
  "If NAME ends in `-mode-hook' (or its name does), return it as a
symbol.  Otherwise, return it as a symbol with `-mode-hook'
appended."
  (intern (+mode-hook-name name)))


;;; Property lists

(defun +plist-merge (&rest plists)
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

(defun +plist-merge-reverse (&rest plists)
  "Create a single property list from all plists in PLISTS.
The process starts by copying the last list, and then setting
properties from the other lists in reverse order.  Settings in
the first list are the most significant ones and overrule
settings in the other lists."
  (apply #'+plist-merge (reverse plists)))


;;; Lists

(defun +move-to-front (elt list)
  "Add/mode ELT to the front of LIST."
  (cons elt (remove elt list)))


;;; Configuration macros

(cl-defmacro +add-hook(hooks &rest body
                             &key fn append local interactive
                             &allow-other-keys)
  "Define a new handler for HOOKS
By default generates a function named `ef-some-hook' if the first hook in
HOOKS is `some-hook'. Usage:

  (+add-hook hook-name/(hook-name...)
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
         (fn (or fn (intern (concat "ef-" hook-name "-h")))))
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
                       `(add-hook ',hook #',fn ,append ,local))
                   hooks)))))

(defmacro +csetq (&rest pairs)
  "For each SYMBOL VALUE pair, calls either `custom-set' or `set-default'."
  (let (forms)
    (while pairs
      (let ((variable (pop pairs))
            (value (pop pairs)))
        (push `(funcall (or (get ',variable 'custom-set) 'set-default)
                        ',variable ,value)
              forms)))
    `(progn ,@(nreverse forms))))


;;; Commands

(defun +update-packages ()
  "Update all packages and freeze versions."
  (interactive)
  (call-interactively #'straight-pull-all)
  (call-interactively #'straight-freeze-versions)
  (call-interactively #'straight-check-all))

(defun +read-file (filename)
  "Return the contents of FILENAME."
  (interactive "f")
  (if (called-interactively-p 'interactive)
      (insert-file-contents filename)
    (with-temp-buffer
      (insert-file-contents filename)
      (buffer-string))))

(defun +insert-form-feed ()
  "Insert form-feed character at point"
  (interactive)
  (insert-char ?\^L))

(defun +insert-unix-time ()
  "Insert UNIX timestamp at point."
  (interactive)
  (insert (format-time-string "%s")))

(defun +insert-uuid ()
  "Insert UUID at point (requires uuidgen to be installed)."
  (interactive)
  (if-let (uuidgen (executable-find "uuidgen"))
      (insert (string-trim (shell-command-to-string uuidgen)))
    (error "Binary 'uuidgen' not found in PATH.")))

(defun +insert-file-name ()
  "Insert current file name at point."
  (interactive)
  (insert (file-name-nondirectory (buffer-file-name))))

(defun +insert-file-name-base ()
  "Insert current file base name at point."
  (interactive)
  (insert (file-name-base (buffer-file-name))))

(defun +insert-file-name-directory ()
  "Insert current file name directory at point."
  (interactive)
  (insert (file-name-directory (buffer-file-name))))

(defun +insert-iso-datetime ()
  "Insert ISO 8601 date/time at point."
  (interactive)
  (insert (concat
	   (format-time-string "%Y-%m-%dT%T")
	   ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
	    (format-time-string "%z")))))

(defun +insert-ordinal-date ()
  "Insert ordinal date at point."
  (interactive)
  (insert (format-time-string "%Y-%j")))

(defun +indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun +kill-buffers-matching (filter)
  "Kill all other buffers matching FILTER, except visible buffers
and the *scratch* buffer.

If FILTER is `nil' kill all except currently visible buffers and the
*scratch* buffer."
  (interactive "sFilter: ")
  (dolist (buf (delq (current-buffer) (buffer-list)))
    (unless (get-buffer-window buf)
      (let ((name (buffer-name buf)))
        (when (and (not (string= name "*scratch*"))
                   (or (not filter)
                       (string-match filter (string-trim name))))
          (if-let ((win (get-buffer-window buf)))
              (delete-window win)
            (kill-buffer buf)))))))

(defun +kill-other-buffers ()
  "Kill all other buffers except special buffers."
  (interactive)
  (+kill-buffers-matching "^[^\\*]"))

(defun +kill-all-other-buffers ()
  "Kill all other buffers except the *scratch* buffer."
  (interactive)
  (+kill-buffers-matching nil))

(defun +delete-frame-or-kill-terminal (&optional arg)
  "Delete frame or save buffers and kill terminal.

If the current frame is not the last frame, call `delete-frame' passing
ARG, otherwise interactively call `save-buffers-kill-terminal'.

With prefix ARG, silently save all file-visiting buffers, then kill."
  (interactive "P")
  (if (cdr (frame-list))
      (progn (save-some-buffers arg)
	     (delete-frame))
    (call-interactively #'save-buffers-kill-terminal t (vector arg))))

(with-eval-after-load 'general
  (general-define-key "C-x C-c" #'+delete-frame-or-kill-terminal))

(provide 'core-lib)
