;;; avdl-mode.el --- AVDL mode for editing Avro Descriptor Language files.

;; Copyright (C) 2013-2021 emacs-avro contributors

;; Author:     2013 Neal Sidhwaney
;; Maintainer: 2021 Luis Osa
;; License: Apache-2.0

(require 'cc-mode)

(eval-and-compile
  (c-add-language 'avdl-mode 'c++-mode))

;; Add types for AVDL language.
(c-lang-defconst c-primitive-type-kwds
  avdl '("bytes" "boolean" "double" "float" "int" "long" 
	 "string" "null" "void"))

;; ;; Modifiers for fields and RPC functions, and annotations.
(c-lang-defconst c-modifier-kwds
  avdl '("oneway" "order" "java-class" "java-key-class"
	 "namespace" "aliases"))

;; Modifiers for RPC function & type declarations.
(c-lang-defconst c-type-modifier-kwds
  avdl '("union" "fixed" "array" "map"))

;; "Class"-like keywords.  Keep the existing ones for things like
;; enum.
(c-lang-defconst c-class-decl-kwds
  avdl '("record" "error" "protocol") )

;; The expresison after ENUM isn't indented correctly if CC-mode
;; considers it a block decl with vars (because it searches for a
;; semicolon in those blocks).  Unfortunately not treating enum like
;; this means we lose font lock for enum constants, for now.
(c-lang-defconst c-block-decls-with-vars
  avdl '("class" "struct" "union" "typedef") )

(defcustom avdl-font-lock-extra-types nil
  "*List of extra types (aside from the type keywords) to recognize
 in AVDL mode.  Each list item should be a regexp matching a single identifier.")

(defconst avdl-font-lock-keywords-1 (c-lang-const c-matchers-1 avdl)
  "Minimal highlighting for AVDL mode.")

(defconst avdl-font-lock-keywords-2 (c-lang-const c-matchers-2 avdl)
  "Fast normal highlighting for AVDL mode.")

(defconst avdl-font-lock-keywords-3 (c-lang-const c-matchers-3 avdl)
  "Accurate normal highlighting for AVDL mode.")

(defvar avdl-font-lock-keywords avdl-font-lock-keywords-3
  "Default expressions to highlight in AVDL mode.")

(defvar avdl-mode-syntax-table nil
  "Syntax table used in avdl-mode buffers.")
(or avdl-mode-syntax-table
    (setq avdl-mode-syntax-table
	  (funcall (c-lang-const c-make-mode-syntax-table c++))))

(defvar avdl-mode-map (let ((map (c-make-inherited-keymap)))
		      ;; Add bindings which are only useful for AVDL
		      map)
  "Keymap used in avdl-mode buffers.")

(easy-menu-define avdl-menu avdl-mode-map "AVDL Mode Commands"
		  ;; Can use `avdl' as the language for `c-mode-menu'
		  ;; since its definition covers any language.  In
		  ;; this case the language is used to adapt to the
		  ;; nonexistence of a cpp pass and thus removing some
		  ;; irrelevant menu alternatives.
		  (cons "AVDL" (c-lang-const c-mode-menu avdl)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.avdl\\'" . avdl-mode))

;;;###autoload
(defun avdl-mode ()
  "Major mode for editing AVDL code.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `avdl-mode-hook'.

Key bindings:
\\{avdl-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table avdl-mode-syntax-table)
  (setq major-mode 'avdl-mode
	mode-name "AVDL")
  (use-local-map c-mode-map)
  ;; `c-init-language-vars' is a macro that is expanded at compile
  ;; time to a large `setq' with all the language variables and their
  ;; customized values for our language.
  (c-init-language-vars avdl-mode)
  ;; `c-common-init' initializes most of the components of a CC Mode
  ;; buffer, including setup of the mode menu, font-lock, etc.
  ;; There's also a lower level routine `c-basic-common-init' that
  ;; only makes the necessary initialization to get the syntactic
  ;; analysis and similar things working.
  (c-common-init 'avdl-mode)
  (easy-menu-add avdl-menu)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'avdl-mode-hook)
  (setq c-basic-offset 4)
  (c-update-modeline))

(provide 'avdl-mode)

;;; derived-mode-ex.el ends here
