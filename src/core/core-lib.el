(require 'cl-macs)
(require 'cl-seq)
(require 'subr-x)

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

(defconst ef-deflang-prefix-handlers
  '(:compile-prefix nil
                    :doc-prefix: nil
                    :eval-prefix nil
                    :lint-prefix nil
                    :macro-prefix nil
                    :repl-prefix nil
                    :test-prefix nil
                    :xref-prefix nil)
  "Prefix handler definitions for prefix keybinds defined in
`ef-deflang-keybinds'.")

(defconst ef-deflang-keybinds
  '((:compile-prefix            ("c" :wk "Compile"))
    (:doc-prefix:               ("cd" :wk "Documentation"))
    (:eval-prefix               ("e" :wk "Eval"))
    (:lint-prefix               ("cl" :wk "Lint"))
    (:macro-prefix              ("m" :wk "Macro"))
    (:refactor-prefix           ("cr" :wk "Refactor"))
    (:repl-prefix               ("r" :wk "REPL"))
    (:specification-prefix      ("s" :wk "Specification"))
    (:test-prefix               ("t" :wk "Test"))
    (:xref-prefix               ("x" :wk "Xref"))

    ;; Actions
    (:compile-buffer            ("cb" :wk "Compile Buffer"))
    (:compile                   ("cc" :wk "Compile All/Project"))
    (:compile-recompile         ("cC" :wk "Re-Compile All/Project"))
    (:compile-defun             ("cd" :wk "Compile Definition at Point"))
    (:compile-file              ("cf" :wk "Compile File"))
    (:compile-region            ("cr" :wk "Compile Region"))
    (:compile-sexp              ("cs" :wk "Compile S-Expression"))
    (:compile-backend-connect   ("cj" :wk "Connect"))
    (:compile-backend-reconnect ("cJ" :wk "Reconnect"))
    (:compile-backend-quit      ("cq" :wk "Quit"))
    (:compile-nav-jump          ("," :wk "Jump to Definition"))
    (:compile-nav-pop-back      ("." :wk "Pop Back"))
    (:doc-apropos               ("cda" :wk "Apropos"))
    (:doc-apropos-select        ("cdA" :wk "Apropos (Select)"))
    (:doc-point                 ("cdk" :wk "Describe Thing at Point"))
    (:doc-guide                 ("cdg" :wk "Open Guide"))
    (:doc-manual                ("cdm" :wk "Open Manual"))
    (:doc-cheatsheet            ("cdc" :wk "Open Cheat Sheet"))
    (:doc-search                ("cds" :wk "Search"))
    (:eval-all                  ("ea" :wk "Eval All/Project"))
    (:eval-buffer               ("eb" :wk "Eval Buffer"))
    (:eval-expression           ("ee" :wk "Eval Expression"))
    (:eval-file                 ("ef" :wk "Eval File"))
    (:eval-defun                ("ed" :wk "Eval Definition at Point"))
    (:eval-region               ("er" :wk "Eval Region"))
    (:eval-sexp                 ("es" :wk "Eval S-Expression"))
    (:lint-file                 ("clf" :wk "Lint File"))
    (:lint-project              ("clp" :wk "Lint Project"))
    (:macro-expand-all          ("me" :wk "Expand All"))
    (:macro-expand-one          ("m1" :wk "Expand One"))
    (:macro-expand-expression   ("mE" :wk "Expand Expression"))
    (:macro-quit                ("mq" :wk "Quit Expansions"))
    (:refactor-rename           ("crr" :wk "Rename"))
    (:repl-context              ("rc" :wk "Change REPL Context"))
    (:repl-info                 ("ri" :wk "REPL Info"))
    (:repl-toggle               ("rr" :wk "Toggle REPL Window"))
    (:repl-quit                 ("r!" :wk "Quit REPL"))
    (:specification-browse      ("csb" :wk "Browse Specifications"))
    (:specification-all         ("csa" :wk "List All Specifications"))
    (:test-all                  ("ta" :wk "Test All/Project"))
    (:test-errors               ("te" :wk "Test Failed Tests"))
    (:test-toggle               ("tl" :wk "Toggle test/implementation"))
    (:test-at-point             ("tp" :wk "Test at Point"))
    (:test-file                 ("tt" :wk "Test File"))
    (:test-report               ("tr" :wk "Show Test Report"))
    (:xref-apropos              ("xa" :wk "Find Symbols"))
    (:xref-definitions          ("xd" :wk "Find Definitions"))
    (:xref-dependencies         ("xD" :wk "Find Dependencies"))
    (:xref-references           ("xr" :wk "Find References")))
  "Keybind definitions for `ef-deflang'")

(use-package gcmh
  :ensure t
  :config
  (gcmh-mode 1))

(use-package which-key
  :ensure t
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
  :ensure t
  :config
  (declare-function general-override-mode "general")
  (general-auto-unbind-keys)
  (general-override-mode t))

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

(defmacro ef-eval-after-load (features &rest body)
  "Evaluate BODY after FEATURES have been loaded by generating
nested `eval-after-load' forms."
  (declare (indent defun))
  (unless (consp features)
    (setq features (list features)))
  (cl-flet ((acc (x xs)
		 `(eval-after-load ',x #',(or xs (macroexp-progn body)))))
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

(defmacro ef-deflang (lang &rest args)
  (declare (indent defun))
  (let ((features (or (ef-as-list (plist-get args :after))
                      (ef-mode lang)))
        (merged-args (ef-plist-merge ef-deflang-prefix-handlers args)))
    (cl-remf args :after)
    (macroexpand
     `(ef-eval-after-load
        ,features
        (general-define-key
         :prefix ef-prefix
         :states '(normal visual)
         :keymaps ',(mapcar #'ef-mode-map (ef-as-list (or (plist-get args :maps)
                                                          (ef-mode-map lang))))
         ,@(cl-loop for (key fn) on merged-args by #'cddr
                    for def = (alist-get key ef-deflang-keybinds)
                    when def
                    collect `(,(caar def) ',`(,fn ,@(cdar def))) into matches
                    finally (return (apply #'append matches))))))))

(provide 'core-lib)
