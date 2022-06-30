;;; core-project.el --- Project management -*- lexical-binding: t; -*-
(require 'core-evil)

(use-package project
  :straight nil
  :commands (+project-find-file)
  :general
  ([remap find-file] '+project-find-file)
  (:states 'normal :prefix ef-leader
   "f"  '(+project-find-file :wk "Find file")
   "p"  '(nil :wk "Project")
   "p!" '(project-shell-command :wk "Run shell command")
   "pf" '(project-find-file :wk "Find file")
   "pF" '(project-forget-project :wk "Forget project")
   "pS" '(+project-shell :wk "Open shell")
   "pd" '(project-find-dir :wk "dired")
   "pD" '(project-dired :wk "dired in root")
   "pv" '(project-vc-dir :wk "Run vc-dir")
   "pk" '(project-kill-buffers :wk "Kill project buffers")
   "pp" '(+project-switch-project :wk "Switch project")
   "ps" '(project-switch-to-buffer :wk "Switch to project buffer"))
  :config
  (defun +project-shell ()
    "Open VTerm Shell in Project Root"
    (interactive)
    (let ((root (project-root (project-current t))))
      (let ((default-directory root)
            (project-current-inhibit-prompt t))
        (vterm))))

  (defun +project-switch-project (dir)
    "\"Switch\" to another project and find file."
    (interactive (list (project-prompt-project-dir)))
    (let ((default-directory dir)
          (project-current-inhibit-prompt t))
      (call-interactively #'project-find-file)))

  (defun +project-root ()
    "Return the current project root or nil if not in a project."
    (when-let ((project (project-current nil)))
      (project-root project)))

  (defun +project-find-file ()
    "If in a project call `project-find-file', otherwise call `find-file'."
    (interactive)
    (if (project-current)
        (project-find-file)
      (call-interactively #'find-file)))

  ;; Support for non-vc backed project roots
  ;;
  ;; https://christiantietze.de/posts/2022/03/mark-local-project.el-directories/
  (defgroup ef-project-local nil
    "Local, non-VC-backed project.el root directories."
    :group 'project)

  (defcustom ef-project-local-identifier
    '(".project"
      ;; Make & CMake
      "Makefile"
      "GNUMakefile"
      "CMakeLists.txt"

      ;; Universal
      "SConstruct"
      "meson.build"

      ;; Bazel
      "WORKSPACE"

      ;; Ruby
      "Rakefile"
      "Gemfile"

      ;; Erlang & Elixir
      "rebar.config"
      "mix.exs"

      ;; Clojure
      "deps.edn"
      "project.clj"
      ".bloop"
      "build.boot"

      ;; Emacs
      "Cask"
      "Eldev"
      "Eldev-local"

      ;; Rust
      "Cargo.toml"

      ;; Racket
      "info.rkt"

      ;; OCaml
      "dune-project"

      ;; Scala
      "build.sbt"
      "build.sc"

      ;; Java etc.
      "pom.xml"
      "build.gradle"
      "gradlew"

      ;; Python
      "requirements.txt"
      "setup.py"
      "tox.ini"
      "Pipfile"
      "poetry.lock"

      ;; Haskell
      "stack.yaml")
    "Filename(s) that identifies a directory as a project.
You can specify a single filename or a list of names."
    :type '(choice (string :tag "Single file")
                   (repeat (string :tag "Filename")))
    :group 'ef-project-local)

  (cl-defmethod project-root ((project (head local)))
    "Return root directory of current PROJECT."
    (cdr project))

  (defun +locate-deepest-file (file name)
    "File the deepest ancestor directory of FILE containing NAME."
    (let ((path (locate-dominating-file file name)))
      (when path
        (let ((parent (file-name-directory (directory-file-name path))))
          (or (+locate-deepest-file parent name) path)))))

  (defun +project-local-try-local (dir)
    "Determine if DIR is a non-VC project.
DIR must include a file with the name determined by the
variable `project-local-identifier' to be considered a project."
    (if-let ((root (if (listp ef-project-local-identifier)
                       (seq-some (lambda (n)
                                   (+locate-deepest-file dir n))
                                 ef-project-local-identifier)
                     (+locate-deepest-file dir ef-project-local-identifier))))
        (cons 'local root)))

  (customize-set-variable 'project-find-functions
                          (list #'project-try-vc
                                #'+project-local-try-local)))

(provide 'core-project)
