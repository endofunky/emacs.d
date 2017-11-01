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
         ("\\.rabl\\'" . ruby-mode)
         ("\\.rake\\'" . ruby-mode)
         ("\\.rb\\'" . ruby-mode)
         ("\\.ru\\'" . ruby-mode)
         ("\\.thor\\'" . ruby-mode))
  :interpreter ("ruby" . ruby-mode)
  :config
  (setq ruby-insert-encoding-magic-comment nil)
  (setq ruby-deep-arglist nil)
  (setq ruby-deep-indent-paren nil)

  (ef-define-repl ef-repl-ruby "*ruby*" 'inf-ruby)
  (evil-define-key 'normal ruby-mode-map ",r" 'ef-repl-ruby)
  (evil-define-key 'normal inf-ruby-mode-map ",r" 'ef-repl-ruby)
  (evil-define-key 'normal ruby-mode-map ",eb" 'ruby-send-buffer)
  (evil-define-key 'visual ruby-mode-map ",er" 'ruby-send-region)

  (ef-shackle '("*rake-compilation*" :align below :size .4 :popup t :select t))

  (defun ef-ruby-mode-hook ()
    (yard-mode 1)
    (eldoc-mode 1)
    (global-rbenv-mode 1)

    (require 'smartparens-ruby)
    (sp-with-modes '(ruby-mode)
      (sp-local-pair "[" nil :post-handlers '((ef-sp-create-newline-and-enter-sexp "RET")))
      (sp-local-pair "{" nil :post-handlers '((ef-sp-create-newline-and-enter-sexp "RET")))
      (sp-local-pair "(" nil :post-handlers '((ef-sp-create-newline-and-enter-sexp "RET")))))

  (add-hook 'ruby-mode-hook 'ef-ruby-mode-hook)

  (defun ef-inf-ruby-mode-hook ()
    (comint-read-input-ring 'silent)
    (make-local-variable 'company-backends)
    (setq company-backends (remq 'company-capf company-backends)))

  (add-hook 'inf-ruby-mode 'ef-inf-ruby-mode-hook))

(use-package bundler
  :after ruby-mode
  :ensure t)

(use-package ruby-interpolation
  :after ruby-mode
  :ensure t
  :diminish ruby-interpolation-mode)

(use-package yard-mode
  :after ruby-mode
  :ensure t
  :diminish yard-mode)

(use-package rbenv
  :after ruby-mode
  :ensure t
  :config
  (global-rbenv-mode 1)
  (setq rbenv-modeline-function 'rbenv--modeline-plain))

(use-package ruby-test-mode
  :after ruby-mode
  :diminish ruby-test-mode
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'ruby-test-mode)
  (evil-define-key 'normal ruby-test-mode-map ",tp" 'ruby-test-run-at-point)

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

(provide 'lang-ruby)
