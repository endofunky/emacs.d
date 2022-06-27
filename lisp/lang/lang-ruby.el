(require 'core-lib)
(require 'core-project)
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
  :interpreter "j?ruby\\(?:[0-9.]+\\)"
  :custom
  (ruby-deep-arglist nil)
  (ruby-deep-indent-paren nil)
  (ruby-insert-encoding-magic-comment nil)
  :general
  (:prefix ef-local-leader :states '(normal visual) :keymaps 'ruby-mode-map
   "c"  '(nil :wk "Compile")
   "cd" '(ruby-disasm :wk "Disassemble"))
  :config
  (add-hook 'ruby-mode-hook 'ef-enable-lsp-maybe)

  ;; Suppress warnings
  (setenv "RUBYOPT" "-W0")

  (ef-add-popup "*rake-compilation*")

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
      ;; Run the Ruby command before `with-current-buffer' since that one will
      ;; have a different `envrc' environment.
      (let ((disasm (shell-command-to-string
                     (format "ruby -e \"STDOUT.puts \
RubyVM::InstructionSequence.compile_file('%s').disasm\"" f))))
        (with-current-buffer buf
          (erase-buffer)
          (insert disasm)
          (goto-char (point-min))
          (asm-mode)))
      (switch-to-buffer buf)
      (delete-file f))))

(use-package inf-ruby
  :after ruby-mode
  :custom
  (inf-ruby-default-implementation "pry")
  :general
  (:prefix ef-local-leader :states '(normal visual) :keymaps 'ruby-mode-map
   "e"  '(nil :wk "Eval")
   "eb" '(ruby-send-buffer :wk "Buffer")
   "ed" '(ruby-send-definition :wk "Definition")
   "el" '(ruby-send-line :wk "Line")
   "er" '(ruby-send-region :wk "Region")

   "r"  '(nil :wk "REPL")
   "rr" '(run-ruby :wk "Open"))
  :hook
  ;; Switch to inf-ruby if a breakpoint has been hit.
  (compilation-filter . inf-ruby-auto-enter)
  :config
  (ef-add-hook inf-ruby-mode-hook
    (comint-read-input-ring 'silent))

  (ef-add-popup 'inf-ruby-mode))

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
  :after ruby-mode)

(use-package ruby-interpolation
  :after ruby-mode)

(use-package rake
  :after ruby-mode
  :commands rake
  :config
  (ef-add-hook rake-compilation-mode-hook
    (setq-local compilation-scroll-output t)))

(use-package ruby-test-mode
  :after ruby-mode
  :functions (ef-file-or-nil
              ef-ruby-test-infer-file
              ruby-test-run
              ruby-test-with-ruby-directory)
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
  :general
  (:prefix ef-local-leader :states '(normal visual) :keymaps 'ruby-mode-map
   "t"  '(nil :wk "Test")
   "tl" '(ruby-test-toggle-implementation-and-specification :wk "Toggle")
   "tp" '(ruby-test-run-at-point :wk "At point")
   "tt" '(ef-ruby-test-run :wk "Buffer"))
  :defines (ruby-test-rspec-options)
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

(use-package rubocop
  :after ruby-mode
  :commands (rubocop-autocorrect-project
             rubocop-autocorrect-current-file)
  :hook
  (ruby-mode . rubocop-mode)
  :general
  (:prefix ef-local-leader :states '(normal visual) :keymaps 'rubocop-mode-map
   "l"  '(nil :wk "Lint")
   "ld" '(rubocop-check-directory :wk "Directory")
   "lb" '(rubocop-check-current-file :wk "Buffer")
   "la" '(rubocop-check-project :wk "Project")

   "la"  '(nil :wk "Auto-correct")
   "lad" '(rubocop-autocorrect-directory :wk "Directory")
   "lab" '(rubocop-autocorrect-current-file :wk "Buffer")
   "laa" '(rubocop-autocorrect-project :wk "Project")

   "lf"  '(nil :wk "Format")
   "lfd" '(rubocop-format-directory :wk "Directory")
   "lfb" '(rubocop-format-current-file :wk "Buffer")
   "lfa" '(rubocop-format-project :wk "Project"))
  :config
  (defun rubocop-buffer-name (file-or-dir)
    "Generate a name for the RuboCop buffer from FILE-OR-DIR."
    "*rubocop*")

  (ef-add-popup "*rubocop*"))

(provide 'lang-ruby)