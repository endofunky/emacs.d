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

  ;; Suppress warnings
  (setenv "RUBYOPT" "-W0")

  (ef-define-repl ef-repl-ruby "*ruby*" 'run-ruby)

  (evil-define-key 'normal ruby-mode-map ",r" 'ef-repl-ruby)
  (evil-define-key 'normal inf-ruby-mode-map ",r" 'ef-repl-ruby)
  (evil-define-key 'normal ruby-mode-map ",eb" 'ruby-send-buffer)
  (evil-define-key 'normal ruby-mode-map ",ed" 'ruby-send-definition)
  (evil-define-key 'visual ruby-mode-map ",er" 'ruby-send-region)
  (evil-define-key 'normal ruby-mode-map ",m" 'yari)

  (ef-shackle '("*rake-compilation*" :align below :size .4 :popup t :select t))

  (ef-add-hook ruby-mode-hook :interactive t
    (require 'smartparens-ruby)
    (sp-with-modes '(ruby-mode)
      (sp-local-pair "[" nil :post-handlers '((ef-sp-create-newline-and-enter-sexp "RET")))
      (sp-local-pair "{" nil :post-handlers '((ef-sp-create-newline-and-enter-sexp "RET")))
      (sp-local-pair "(" nil :post-handlers '((ef-sp-create-newline-and-enter-sexp "RET"))))
    (if (file-exists-p (expand-file-name ".solargraph.yml" (projectile-project-root)))
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
  :ensure t
  :diminish ruby-interpolation-mode)

(use-package rake
  :commands rake
  :config
  (ef-add-hook rake-compilation-mode-hook
    (setq-local compilation-scroll-output t)))

(use-package ruby-test-mode
  :after ruby-mode
  :diminish ruby-test-mode
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'ruby-test-mode)
  (setq ruby-test-rspec-options "")
  (evil-define-key 'normal ruby-test-mode-map ",tp" 'ruby-test-run-at-point)
  (evil-define-key 'normal ruby-test-mode-map ",o" 'ruby-test-toggle-implementation-and-specification)

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
            (ruby-test-with-ruby-directory testname
                                           (ruby-test-run-command (ruby-test-command testname)))
          (message "no corresponding test/spec file found")))))

  (evil-define-key 'normal ruby-test-mode-map ",tt" 'ef-ruby-test-run))

(use-package projectile-rails
  :after ruby-mode
  :diminish projectile-rails-mode
  :ensure t
  :init
  (ef-define-repl ef-repl-projectile-rails-console "*rails*" #'(lambda () (projectile-rails-console nil)))
  :config
  (projectile-rails-global-mode t)
  (evil-define-key 'normal projectile-rails-mode-map ",r" 'ef-repl-projectile-rails-console))

(use-package ruby-tools
  :after ruby-mode
  :ensure t
  :diminish ruby-tools-mode
  :init
  (add-hook 'ruby-mode-hook 'ruby-tools-mode)
  :config
  (evil-define-key 'normal ruby-mode-map ",x:" 'ruby-tools-to-symbol)
  (evil-define-key 'normal ruby-mode-map ",x\"" 'ruby-tools-to-double-quote-string)
  (evil-define-key 'normal ruby-mode-map ",x\'" 'ruby-tools-to-single-quote-string))

(use-package yari
  :commands yari
  :load-path "vendor/yari.el"
  :config
  (ef-shackle '(yari-mode :align below :size .4 :popup t :select t))
  (evil-define-key 'normal yari-mode-map "q" 'quit-window))

(use-package rubocop
  :ensure t
  :commands (rubocop-autocorrect-project
             rubocop-autocorrect-current-file)
  :init
  (evil-define-key 'normal ruby-mode-map ",clf" 'rubocop-autocorrect-current-file)
  (evil-define-key 'normal ruby-mode-map ",clp" 'rubocop-autocorrect-project)
  :config
  (defun rubocop-buffer-name (file-or-dir)
    "Generate a name for the RuboCop buffer from FILE-OR-DIR."
    "*rubocop*")
  (ef-shackle '("*rubocop*" :align below :size .4 :popup t :select t)))

(defun ef-spring-server ()
  (interactive)
  (when-let* ((root (projectile-project-root))
              (cmd (format "cd %s && %s" root  (concat
                                                (file-name-as-directory "bin")
                                                "spring server"))))
    (message "Running spring server: %s" cmd)
    (async-shell-command cmd "*spring-server*")))

(provide 'lang-ruby)
