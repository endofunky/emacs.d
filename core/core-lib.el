(require 'cl-macs)
(require 'cl-seq)
(require 'subr-x)

(use-package general
  :ensure t
  :config
  (declare-function general-override-mode "general")
  (general-auto-unbind-keys)
  (general-override-mode t))

(use-package transient
  :ensure t
  :general
  (:keymaps '(transient-map transient-edit-map)
            "<escape>" 'transient-quit-all
            "?" 'transient-show
            "C-h" 'transient-show
            "C-t" 'transient-help)
  :commands (transient-define-prefix)
  :custom
  (transient-enable-popup-navigation t)
  (transient-show-popup 1))

(defgroup ef-deflang nil
  "Endomacs deflang configuration."
  :group 'startup)

(defcustom ef-deflang-form-regexp-eval
  `(concat ,(eval-when-compile
              (concat "^\\s-*("
                      (regexp-opt '("ef-deflang") t)
                      "\\s-+\\("))
           (or (bound-and-true-p lisp-mode-symbol-regexp)
               "\\(?:\\sw\\|\\s_\\|\\\\.\\)+") "\\)")
  "Sexp providing regexp for finding ef-deflang forms in user files."
  :type 'sexp
  :group 'ef-deflang)

(defcustom ef-deflang-enable-imenu-support t
  "If non-nil, cause `imenu' to see `ef-deflang' declarations.
This is done by adjusting `lisp-imenu-generic-expression' to include
support for finding `ef-deflang'.

Must be set before loading ef-deflang."
  :type 'boolean
  :set
  #'(lambda (_sym value)
      (eval-after-load 'lisp-mode
        (if value
            `(add-to-list 'lisp-imenu-generic-expression
                          (list "Languages" ,ef-deflang-form-regexp-eval 2))
          `(setq lisp-imenu-generic-expression
                 (remove (list "Languages" ,ef-deflang-form-regexp-eval 2)
                         lisp-imenu-generic-expression)))))
  :group 'ef-deflang)

(defgroup ef-keybinds nil
  "Endomacs keybinds."
  :group 'faces)

(defcustom ef-prefix ","
  "Prefix leader used for endomacs key-bindings."
  :group 'ef-theme
  :type 'string)

(defgroup ef-theme nil
  "Endomacs faces."
  :group 'faces)

(defcustom ef-fullscreen-indicator "orange red"
  "Set foreground color for fullscreen indicator in mode-line."
  :group 'ef-theme
  :type 'string)

(defconst ef-deflang-defaults '())

(defconst ef-deflang-compile-defs
  '((:compile-buffer       (:key "b"  :desc "Compile Buffer"))
    (:compile              (:key "c"  :desc "Compile All/Project"))
    (:compile-defun        (:key "d"  :desc "Compile Definition at Point"))
    (:compile-file         (:key "f"  :desc "Compile File"))
    (:compile-region       (:key "r"  :desc "Compile Region"))
    (:compile-sexp         (:key "s"  :desc "Compile S-Expression"))))

(defconst ef-deflang-compile-backend-defs
  '((:compile-backend-connect   (:key "j"  :desc "Connect"))
    (:compile-backend-reconnect (:key "J"  :desc "Reconnect"))
    (:compile-backend-quit      (:key "q"  :desc "Quit"))))

(defconst ef-deflang-compile-nav-defs
  '((:compile-nav-jump     (:key ","  :desc "Jump to Definition"))
    (:compile-nav-pop-back (:key "."  :desc "Pop Back"))))

(defconst ef-deflang-doc-defs
  '((:doc-apropos          (:key "a"  :desc "Apropos"))
    (:doc-apropos-select   (:key "A"  :desc "Apropos (Select)"))
    (:doc-point            (:key "k"  :desc "Describe Thing at Point"))
    (:doc-manual           (:key "m"  :desc "Open Manual"))
    (:doc-search           (:key "s"  :desc "Search Manual"))))

(defconst ef-deflang-eval-defs
  '((:eval-all             (:key "a"  :desc "Eval All/Project"))
    (:eval-buffer          (:key "b"  :desc "Eval Buffer"))
    (:eval-expression      (:key "e"  :desc "Eval Expression"))
    (:eval-file            (:key "f"  :desc "Eval File"))
    (:eval-defun           (:key "d"  :desc "Eval Definition at Point"))
    (:eval-region          (:key "r"  :desc "Eval Region"))
    (:eval-sexp            (:key "s"  :desc "Eval S-Expression"))))

(defconst ef-deflang-lint-defs
  '((:lint-file            (:key "f"  :desc "Lint File"))
    (:lint-project         (:key "p"  :desc "Lint Project"))))

(defconst ef-deflang-macroexp-defs
  '((:macro-expand-all        (:key "e"  :desc "Expand All"))
    (:macro-expand-one        (:key "1"  :desc "Expand One"))
    (:macro-expand-expression (:key "E"  :desc "Expand Expression"))
    (:macro-quit              (:key "q"  :desc "Quit Expansions"))))

(defconst ef-deflang-test-defs
  '((:test-all             (:key "a"  :desc "Test All/Project"))
    (:test-toggle          (:key "l"  :desc "Toggle test/implementation"))
    (:test-at-point        (:key "p"  :desc "Test at Point"))
    (:test-file            (:key "t"  :desc "Test File"))
    (:test-report          (:key "r"  :desc "Show Test Report"))))

(defconst ef-deflang-compile-menu-defs
  '((:compile-menu-doc      (:key "d"  :desc "Documentation"))
    (:compile-menu-eval     (:key "e"  :desc "Evaluate"))
    (:compile-menu-lint     (:key "l"  :desc "Lint"))
    (:compile-menu-manual   (:key "m"  :desc "Manual/Documentation"))
    (:compile-menu-test     (:key "t"  :desc "Test"))
    (:compile-menu-macroexp (:key "x"  :desc "Macro"))))

(defconst ef-deflang-keybinds
  '((:compile-menu . "c")
    (:compile-menu-eval . "e")
    (:compile-menu-test . "t")
    (:compile-menu-macroexp . "x")
    (:compile-nav-jump . ",")
    (:compile-nav-pop-back . ".")
    (:test-toggle . "l")))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; lists
;;

(cl-defun ef-split-list (list parts &key (last-part-longer nil))
  "Split LIST into PARTS parts.  They will all be the same length
except the last one which will be shorter or, if LAST-PART-LONGER
is true, longer.  Doesn't deal with the case where there are less
than PARTS elements in LIST at all (it does something, but it may
not be sensible). "
  (cl-loop with size = (if last-part-longer
                           (floor (length list) parts)
                         (ceiling (length list) parts))
           and tail = list
           for part upfrom 1
           while tail
           collect (cl-loop for pt on tail
                            for i upfrom 0
                            while (or (and last-part-longer (= part parts))
                                      (< i size))
                            collect (car pt)
                            finally (setf tail pt))))

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

(defmacro ef-eval-after-load (features &rest body)
  "Evaluate BODY after FEATURES have been loaded by generating
nested `eval-after-load' forms."
  (declare (indent defun))
  (unless (consp features)
    (setq features (list features)))
  (cl-flet ((acc (x xs)
		 `(eval-after-load ',x ',(or xs (macroexp-progn body)))))
    (cl-reduce #'acc features :initial-value '() :from-end t)))

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
     (unless (memq 'endomacs-customize custom-known-themes)
       (deftheme endomacs-customize)
       (enable-theme 'endomacs-customize)
       (setq custom-enabled-themes (remq 'endomacs-customize custom-enabled-themes)))
     (custom-theme-set-variables
      'endomacs-customize
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
    (if (get-register :ef-fullscreen)
        (progn
          (jump-to-register :ef-fullscreen)
          (set-register :ef-fullscreen nil)
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

(defun ef-align-to-= (begin end)
  "Align region to = signs"
  (interactive "r")
  (align-regexp begin end "\\(\\s-*\\)=" 1 1 ))

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
  "Kill all other buffers matching FILTER.

If FILTER is `nil' kill all buffers except the current one."
  (interactive "sFilter: ")
  (dolist (buf (delq (current-buffer) (buffer-list)))
    (when (or (not filter)
              (string-match filter (string-trim (buffer-name buf))))
      (if-let ((win (get-buffer-window buf)))
          (delete-window win))
      (kill-buffer buf))))

(defun ef-kill-other-buffers ()
  "Kill all other buffers except special buffers."
  (interactive)
  (ef-kill-buffers-matching "^[^\\*]"))

(defun ef-kill-all-other-buffers ()
  "Kill all other buffers except special buffers."
  (interactive)
  (ef-kill-buffers-matching nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Language mode definition
;;

(defsubst ef-deflang-dispatch-name (lang dispatch)
  (intern (format "%s/%s" lang dispatch)))

(defun ef-deflang-match-defs (keydefs args)
  (cl-loop for (keydef . rest) on keydefs
           when (plist-get args (car keydef))
           collect keydef into matched-defs
           finally (return matched-defs)))

(defun ef-deflang-actions (keydefs args)
  (mapcar (lambda (element)
            (if-let* ((key (car element))
                      (keydef (car (cdr element)))
                      (fn (plist-get args key)))
                (list (plist-get keydef :key)
                      (plist-get keydef :desc)
                      fn)))
          keydefs))

(defun ef-deflang-build-menu (lang keydefs name args)
  (when-let* ((defs (ef-deflang-match-defs keydefs args))
              (actions (mapcar #'vconcat
                               (ef-split-list
                                (ef-deflang-actions defs args)
                                3)))
              (menu-name (intern (format ":compile-menu-%s" name)))
              (mode (ef-mode lang))
              (dispatch (ef-deflang-dispatch-name lang name)))
    (eval `(transient-define-prefix ,dispatch ()
             ,(format "Compile (%s) commands for %s." name mode)
             ["Actions"
              ,@actions]))
    (plist-put args menu-name dispatch))
  args)

(defun ef-deflang-build-top-level (lang args)
  (let* ((mode (ef-mode lang))
         (compile-keydefs (ef-deflang-match-defs ef-deflang-compile-defs args))
         (compile-backend-keydefs (ef-deflang-match-defs ef-deflang-compile-backend-defs args))
         (compile-nav-keydefs (ef-deflang-match-defs ef-deflang-compile-nav-defs args))
         (compile-menu-keydefs (ef-deflang-match-defs ef-deflang-compile-menu-defs args))
         (actions (mapcar #'vconcat
                          (ef-split-list
                           (ef-deflang-actions compile-keydefs args)
                           3)))
         (backend-actions (mapcar #'vconcat
                                  (ef-split-list
                                   (ef-deflang-actions compile-backend-keydefs args)
                                   3)))
         (nav-actions (mapcar #'vconcat
                                  (ef-split-list
                                   (ef-deflang-actions compile-nav-keydefs args)
                                   3)))
         (menu-actions (mapcar #'vconcat
                               (ef-split-list
                                (ef-deflang-actions compile-menu-keydefs args)
                                3)))
         (dispatch (ef-deflang-dispatch-name lang "compile")))
    (eval `(transient-define-prefix ,dispatch ()
             ,(format "Run code commands for %s." mode)
             ["Actions"
              ,@actions]
             ["Navigation"
              ,@nav-actions]
             ["Backend"
              ,@backend-actions]
             ["Commands"
              ,@menu-actions]))
    (plist-put args :compile-menu dispatch))
  args)

(defun ef-deflang-bind-keys (lang args)
  (dolist (def ef-deflang-keybinds)
    (when-let* ((menu (car def))
                (key (cdr def))
                (maps (ef-as-list (or (plist-get args :maps)
                                      (ef-mode-map lang))))
                (dispatch (plist-get args menu)))
      (dolist (map maps)
        (general-define-key :states '(normal visual)
                            :keymaps map
                            :prefix ef-prefix
                            key dispatch))))
  args)

(defun ef-deflang-build (lang args)
  (thread-last args
    (ef-plist-merge ef-deflang-defaults)
    (ef-deflang-build-menu lang ef-deflang-doc-defs 'doc)
    (ef-deflang-build-menu lang ef-deflang-eval-defs 'eval)
    (ef-deflang-build-menu lang ef-deflang-lint-defs 'lint)
    (ef-deflang-build-menu lang ef-deflang-macroexp-defs 'macroexp)
    (ef-deflang-build-menu lang ef-deflang-test-defs 'test)
    (ef-deflang-build-top-level lang)
    (ef-deflang-bind-keys lang)))

(defmacro ef-deflang (lang &rest args)
  (declare (indent defun))
  (let* ((features (or (ef-as-list (plist-get args :after))
                       (ef-mode lang))))
    (cl-remf args :after)
    (macroexpand
     `(ef-eval-after-load ,features (ef-deflang-build ',lang ',args)))))

(provide 'core-lib)
