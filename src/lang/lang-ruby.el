(require 'core-lib)
(require 'core-parens)
(require 'core-projectile)
(require 'core-shackle)

(use-package ruby-mode
  :mode (("Appraisals\\'" . ruby-mode)
         ("Berksfile\\'" . ruby-mode)
         ("Brewfile\\'" . ruby-mode)
         ("Capfile\\'" . ruby-mode)
         ("Gemfile\\'" . ruby-mode)
         ("Guardfile\\'" . ruby-mode)
         ("Podfile\\'" . ruby-mode)
         ("Puppetfile\\'" . ruby-mode)
         ("Rakefile\\'" . ruby-mode)
         ("Thorfile\\'" . ruby-mode)
         ("Vagrantfile\\'" . ruby-mode)
         ("\\.builder\\'" . ruby-mode)
         ("\\.cap\\'" . ruby-mode)
         ("\\.gemspec\\'" . ruby-mode)
         ("\\.jbuilder\\'" . ruby-mode)
         ("\\.podspec\\'" . ruby-mode)
         ("\\.pryrc\\'" . ruby-mode)
         ("\\.rabl\\'" . ruby-mode)
         ("\\.rake\\'" . ruby-mode)
         ("\\.rb\\'" . ruby-mode)
         ("\\.ru\\'" . ruby-mode)
         ("\\.thor\\'" . ruby-mode))
  :interpreter ("ruby" . ruby-mode)
  :custom
  (inf-ruby-default-implementation "pry")
  :config
  (setq ruby-insert-encoding-magic-comment nil)
  (setq ruby-deep-arglist nil)
  (setq ruby-deep-indent-paren nil)

  (ef-add-popup "*ruby*")

  ;; Suppress warnings
  (setenv "RUBYOPT" "-W0")

  (ef-add-popup "*rake-compilation*")

  (ef-add-hook ruby-mode-hook :interactive t
    (require 'smartparens-ruby)

    (sp-with-modes '(ruby-mode)
      (sp-local-pair "[" nil :post-handlers '((ef-sp-create-newline-and-enter-sexp "RET")))
      (sp-local-pair "{" nil :post-handlers '((ef-sp-create-newline-and-enter-sexp "RET")))
      (sp-local-pair "(" nil :post-handlers '((ef-sp-create-newline-and-enter-sexp "RET"))))

    (direnv-update-directory-environment)

    (if (locate-file "solargraph" exec-path exec-suffixes 1)
        (lsp)))

  (ef-add-hook inf-ruby-mode-hook
    (comint-read-input-ring 'silent)))

(use-package hideshow
  :after ruby-mode
  :config
  (add-to-list 'hs-special-modes-alist
               `(ruby-mode
                 ,(rx (or "def" "class" "module" "do" "{" "["))
                 ,(rx (or "}" "]" "end"))
                 ,(rx (or "#" "=begin"))
                 ruby-forward-sexp nil)))

(use-package bundler
  :after ruby-mode
  :ensure t)

(use-package ruby-interpolation
  :after ruby-mode
  :ensure t)

(use-package rake
  :commands rake
  :config
  (ef-add-hook rake-compilation-mode-hook
    (setq-local compilation-scroll-output t)))

(use-package ruby-test-mode
  :after ruby-mode
  :ensure t
  :functions (ef-file-or-nil
              ef-ruby-test-infer-file)
  :commands (ef-ruby-test-run
             ruby-test-specification-filename
             ruby-test-toggle-implementation-and-specification
             ruby-test-unit-filename
             ruby-test-default-test-filename
             ruby-test-any-p
             ruby-test-rails-root
             ruby-test-ruby-root
             ruby-test-run-command
             ruby-test-command)
  :config
  (add-hook 'ruby-mode-hook 'ruby-test-mode)
  (setq ruby-test-rspec-options "")

  (defun ef-file-or-nil (filename)
    "Return `filename' if `file-exists-p' returns non-nil, else nil"
    (if (file-exists-p filename)
        filename
      nil))

  (defun ef-ruby-test-infer-file (filename)
    "Return the inferred test or spec for `filename', or nil if it doesn't
exist"
    (cl-some #'ef-file-or-nil (list (ruby-test-specification-filename filename)
                                    (ruby-test-unit-filename filename)
                                    (ruby-test-default-test-filename filename))))

  (defun ef-ruby-test-run ()
    "Run the current test/spec or the test/spec corresponding to the
current buffer's file, if it exists"
    (interactive)
    (let ((filename (buffer-file-name (current-buffer))))
      (if (ruby-test-any-p filename)
          (ruby-test-run)
        (if-let* ((testname (ef-ruby-test-infer-file filename)))
            (ruby-test-with-ruby-directory
             testname
             (ruby-test-run-command (ruby-test-command testname)))
          (message "no corresponding test/spec file found"))))))

(use-package projectile-rails
  :after ruby-mode
  :ensure t
  :config
  (ef-add-popup "*rails*")
  (projectile-rails-global-mode t))

(use-package rubocop
  :ensure t
  :commands (rubocop-autocorrect-project
             rubocop-autocorrect-current-file)
  :config
  (defun rubocop-buffer-name (file-or-dir)
    "Generate a name for the RuboCop buffer from FILE-OR-DIR."
    "*rubocop*")
  (ef-add-popup "*rubocop*"))

(defun ef-spring-server ()
  (interactive)
  (when-let* ((root (projectile-project-root))
              (cmd (format "cd %s && %s" root  (concat
                                                (file-name-as-directory "bin")
                                                "spring server"))))
    (message "Running spring server: %s" cmd)
    (async-shell-command cmd "*spring-server*")))

(defun ruby-disasm (beginning end)
  "Disassemble the selected region or contents of current buffer into YARV
byte-code and display it in an `asm-mode' buffer."
  (interactive "r")
  (let ((f (make-temp-file "ruby-disasm"))
        (buf (or (get-buffer "*ruby-disasm*")
                 (generate-new-buffer "*ruby-disasm*"))))
    (if (use-region-p)
        (append-to-file beginning end f)
      (append-to-file (point-min) (point-max) f))
    (with-current-buffer buf
      (erase-buffer)
      (insert
       (shell-command-to-string
        (format "ruby -e \"STDOUT.puts \
RubyVM::InstructionSequence.compile_file('%s').disasm\"" f)))
      (goto-char (point-min))
      (asm-mode))
    (switch-to-buffer buf)
    (delete-file f)))

(ef-deflang ruby
  ;; compile
  :compile-disassemble ruby-disasm

  ;; eval
  :eval-buffer ruby-send-buffer
  :eval-defun ruby-send-definition
  :eval-region ruby-send-region

  ;; lint
  :lint-file rubocop-autocorrect-current-file
  :lint-project rubocop-autocorrect-project

  ;; repl
  :repl-toggle run-ruby

  ;; test
  :test-toggle ruby-test-toggle-implementation-and-specification
  :test-at-point ruby-test-run-at-point
  :test-file ef-ruby-test-run)

(provide 'lang-ruby)
