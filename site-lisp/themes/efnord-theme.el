;; A modified version of the Nord color theme for emacs:
;;   https://github.com/arcticicestudio/nord-emacs
;;
;; Changes include darker background colors all around, removed some modes I
;; don't use and changed font-lock rules for syntax highlighting to be (almost)
;; monochromatic.
;;
(deftheme efnord
  "A darker, minimalist Nord-theme variation")

(defgroup efnord
  nil
  "A darker, minimalist Nord-theme variation"
  :group 'faces)

(let* ((class '((class color) (min-colors 89)))
       (default       "#ABB2BF")

       (black0        "#191D23")
       (black1        "#171A20")
       (black2        "#1E222A")

       (polar-night-1 "#2E3440")
       (polar-night-2 "#3B4252")
       (polar-night-3 "#434C5E")
       (polar-night-4 "#4C566A")
       (polar-night-5 "#5f6c85")
       (polar-night-6 "#7b88a1")

       (snow-storm-1  "#D8DEE9")
       (snow-storm-2  "#E5E9F0")
       (snow-storm-3  "#ECEFF4")

       (frost-1       "#8FBCBB")
       (frost-2       "#88C0D0")
       (frost-3       "#81A1C1")
       (frost-4       "#5E81AC")

       (aurora-red    "#BF616A")
       (aurora-orange "#D08770")
       (aurora-yellow "#EBCB8B")
       (aurora-green  "#A3BE8C")
       (aurora-purple "#B48EAD")

       (background    black2)
       (comment       polar-night-5)
       (hl-line       polar-night-1)
       (mode-line     black1))

  (custom-theme-set-faces
   'efnord
   `(bold ((,class (:weight bold))))
   `(bold-italic ((,class (:weight bold :slant italic))))
   `(border ((,class (:foreground ,snow-storm-1))))
   `(cursor ((,class (:background ,snow-storm-1))))
   `(default ((,class (:foreground ,default :background ,background))))
   `(fringe ((,class (:foreground ,snow-storm-1 :background ,background))))
   `(italic ((,class (:slant italic))))
   `(match ((,class (:inherit isearch))))
   `(region ((,class (:background ,polar-night-3))))
   `(scroll-bar ((,class (:background ,polar-night-4))))
   `(shadow ((,class (:foreground ,polar-night-4))))
   `(underline ((,class (:underline t))))
   `(vertical-border ((,class (:foreground ,polar-night-3))))

   ;; Misc UI
   `(buffer-menu-buffer ((,class (:foreground ,snow-storm-1 :weight bold))))
   `(button ((,class (:background ,black2 :foreground ,frost-2 :box (:line-width 2 :color ,snow-storm-1 :style sunken-button)))))
   `(file-name-shadow ((,class (:inherit shadow))))
   `(header-line ((,class (:inherit mode-line-inactive))))
   `(help-argument-name ((,class (:foreground ,frost-2))))
   `(highlight ((,class (:foreground ,frost-2 :background ,polar-night-2))))
   `(hl-line ((,class (:background ,hl-line))))
   `(info-menu-star ((,class (:foreground ,frost-3))))
   `(message-separator ((,class (:inherit shadow))))
   `(mm-command-output ((,class (:foreground ,frost-2))))
   `(next-error ((,class (:inherit error))))
   `(nobreak-space ((,class (:foreground ,polar-night-4))))
   `(query-replace ((,class (:foreground ,frost-2 :background ,polar-night-3))))
   `(secondary-selection ((,class (:background ,polar-night-3))))
   `(tool-bar ((,class (:foreground ,snow-storm-1 :background ,polar-night-4))))

   ;; Syntax (Monochrome)
   ;; `(font-lock-builtin-face ((,class (:foreground ,snow-storm-1 :background nil))))
   ;; `(font-lock-comment-face ((,class (:foreground ,comment :background nil))))
   ;; `(font-lock-constant-face ((,class (:foreground ,default :background nil))))
   ;; `(font-lock-function-name-face ((,class (:foreground ,default :background nil))))
   ;; `(font-lock-keyword-face ((,class (:foreground ,snow-storm-2 :background nil))))
   ;; `(font-lock-string-face ((,class (:foreground ,frost-3 :background nil))))
   ;; `(font-lock-doc-face ((,class (:inherit font-lock-comment-face :background nil))))
   ;; `(font-lock-type-face ((,class (:foreground ,default :background nil))))
   ;; `(font-lock-variable-name-face ((,class (:foreground ,default :background nil))))
   ;; `(font-lock-warning-face ((,class (:foreground ,default :background nil))))
   ;; `(font-lock-preprocessor-face ((,class (:foreground ,default :background nil :slant italic))))

   ;; Syntax (Nord)
   `(font-lock-builtin-face ((,class (:foreground ,frost-3))))
   `(font-lock-comment-face ((,class (:foreground ,comment))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,comment))))
   `(font-lock-constant-face ((,class (:foreground ,frost-3))))
   `(font-lock-doc-face ((,class (:foreground ,comment))))
   `(font-lock-function-name-face ((,class (:foreground ,frost-2))))
   `(font-lock-keyword-face ((,class (:foreground ,frost-3))))
   `(font-lock-negation-char-face ((,class (:foreground ,frost-3))))
   `(font-lock-preprocessor-face ((,class (:foreground ,frost-4 :weight bold))))
   `(font-lock-reference-face ((,class (:foreground ,frost-3))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,aurora-yellow))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,aurora-yellow))))
   `(font-lock-string-face ((,class (:foreground ,aurora-green))))
   `(font-lock-type-face ((,class (:foreground ,frost-1))))
   `(font-lock-variable-name-face ((,class (:foreground ,snow-storm-1))))
   `(font-lock-warning-face ((,class (:foreground ,default))))
   `(rust-string-interpolation-face ((,class (:inherit font-lock-string-face))))

   ;; which-key
   `(which-key-face ((,class (:foreground ,default))))
   `(which-key-command-description-face ((,class (:foreground ,snow-storm-1))))
   `(which-key-group-description-face ((,class (:foreground ,aurora-purple))))

   ;; Escape and prompt faces
   `(escape-glyph ((,class (:foreground ,aurora-orange))))
   `(minibuffer-prompt ((,class (:foreground ,frost-2 :weight bold))))

   ;; Alerts
   `(success ((,class (:foreground ,aurora-green))))
   `(warning ((,class (:foreground ,aurora-orange :weight bold))))
   `(error ((,class (:foreground ,aurora-red :weight bold))))

   ;; diff
   `(diff-added ((,class (:foreground ,aurora-green))))
   `(diff-changed ((,class (:foreground ,aurora-yellow))))
   `(diff-context ((,class (:inherit default))))
   `(diff-file-header ((,class (:foreground ,frost-2))))
   `(diff-function ((,class (:foreground ,frost-1))))
   `(diff-header ((,class (:foreground ,frost-3 :weight bold))))
   `(diff-hunk-header ((,class (:foreground ,frost-3 :background ,black2))))
   `(diff-indicator-added ((,class (:foreground ,aurora-green))))
   `(diff-indicator-changed ((,class (:foreground ,aurora-yellow))))
   `(diff-indicator-removed ((,class (:foreground ,aurora-red))))
   `(diff-nonexistent ((,class (:foreground ,aurora-red))))
   `(diff-refine-added ((,class (:foreground ,aurora-green))))
   `(diff-refine-changed ((,class (:foreground ,aurora-yellow))))
   `(diff-refine-removed ((,class (:foreground ,aurora-red))))
   `(diff-removed ((,class (:foreground ,aurora-red))))

   ;; native line numbers (emacs 26)
   `(line-number ((,class (:foreground ,polar-night-2 :background ,background))))
   `(line-number-current-line ((,class (:foreground ,snow-storm-1 :background ,background))))

   ;; Custom
   `(custom-button ((,class (:background ,black2 :foreground ,frost-2 :box (:line-width 2 :color ,snow-storm-1 :style sunken-button)))))
   `(custom-button-mouse ((,class (:background ,snow-storm-1 :foreground ,black2 :box (:line-width 2 :color ,snow-storm-1 :style sunken-button)))))
   `(custom-button-pressed ((,class (:background ,snow-storm-3 :foreground ,black2 :box (:line-width 2 :color ,snow-storm-1 :style sunken-button)))))
   `(custom-button-pressed-unraised ((,class (:background ,snow-storm-1 :foreground ,black2 :box (:line-width 2 :color ,snow-storm-1 :style sunken-button)))))
   `(custom-button-unraised ((,class (:background ,black2 :foreground ,frost-2 :box (:line-width 2 :color ,snow-storm-1 :style sunken-button)))))
   `(custom-changed ((,class (:foreground ,aurora-yellow))))
   `(custom-comment ((,class (:foreground ,comment))))
   `(custom-comment-tag ((,class (:foreground ,frost-1))))
   `(custom-documentation ((,class (:foreground ,snow-storm-1))))
   `(custom-group-tag ((,class (:foreground ,frost-2 :weight bold))))
   `(custom-group-tag-1 ((,class (:foreground ,frost-2 :weight bold))))
   `(custom-invalid ((,class (:foreground ,aurora-red))))
   `(custom-modified ((,class (:foreground ,aurora-yellow))))
   `(custom-rogue ((,class (:foreground ,aurora-orange :background ,polar-night-3))))
   `(custom-saved ((,class (:foreground ,aurora-green))))
   `(custom-set ((,class (:foreground ,frost-2))))
   `(custom-state ((,class (:foreground ,aurora-green))))
   `(custom-themed ((,class (:foreground ,frost-2 :background ,polar-night-3))))

   ;; Message
   `(message-cited-text ((,class (:foreground ,snow-storm-1))))
   `(message-header-cc ((,class (:foreground ,frost-3))))
   `(message-header-name ((,class (:foreground ,frost-1))))
   `(message-header-newsgroup ((,class (:foreground ,aurora-green))))
   `(message-header-other ((,class (:foreground ,snow-storm-1))))
   `(message-header-subject ((,class (:foreground ,frost-2))))
   `(message-header-to ((,class (:foreground ,frost-3))))
   `(message-header-xheader ((,class (:foreground ,aurora-yellow))))
   `(message-mml ((,class (:foreground ,frost-4))))

   ;; Modeline
   `(mode-line ((,class (:foreground ,snow-storm-1 :background ,mode-line))))
   `(mode-line-buffer-id ((,class (:weight bold))))
   `(mode-line-highlight ((,class (:inherit highlight))))
   `(mode-line-inactive ((,class (:foreground ,comment :background ,mode-line))))

   ;; completion
   `(completions-annotations ((,class (:foreground ,frost-3))))
   `(completions-common-part ((,class (:foreground ,frost-2 :weight bold))))
   `(completions-first-difference ((,class (:foreground ,aurora-red))))

   ;; which-function
   `(which-func ((,class (:foreground ,frost-2))))

   ;; show-paren
   `(show-paren-match ((,class (:foreground ,aurora-purple :background unspecified))))
   `(show-paren-mismatch ((,class (:foreground ,aurora-red :background unspecified))))

   ;; tooltips
   `(tooltip ((,class (:foreground ,black2 :background ,snow-storm-1))))

   ;; link faces
   `(link ((,class (:underline t))))
   `(link-visited ((,class (:underline t))))

   ;; isearch
   `(isearch ((,class (:foreground ,black2 :background ,frost-2))))
   `(isearch-fail ((,class (:foreground ,aurora-red))))

   ;; term
   `(term ((,class (:foreground ,snow-storm-1 :background ,black2))))
   `(term-color-black ((,class (:foreground ,polar-night-2 :background ,polar-night-2))))
   `(term-color-white ((,class (:foreground ,snow-storm-2 :background ,snow-storm-2))))
   `(term-color-cyan ((,class (:foreground ,frost-1 :background ,frost-1))))
   `(term-color-blue ((,class (:foreground ,frost-2 :background ,frost-2))))
   `(term-color-red ((,class (:foreground ,aurora-red :background ,aurora-red))))
   `(term-color-yellow ((,class (:foreground ,aurora-yellow :background ,aurora-yellow))))
   `(term-color-green ((,class (:foreground ,aurora-green :background ,aurora-green))))
   `(term-color-magenta ((,class (:foreground ,aurora-purple :background ,aurora-purple))))
   `(tty-menu-disabled-face ((,class (:foreground ,polar-night-2))))
   `(tty-menu-enabled-face ((,class (:background ,polar-night-3 foreground ,snow-storm-1))))
   `(tty-menu-selected-face ((,class (:foreground ,frost-2 :underline t))))

   ;; wid-edit
   `(widget-button-pressed ((,class (:foreground ,frost-3 :background ,polar-night-2))))
   `(widget-documentation ((,class (:foreground ,snow-storm-1))))
   `(widget-field ((,class (:background ,polar-night-3 :foreground ,snow-storm-1))))
   `(widget-single-line-field ((,class (:background ,polar-night-3 :foreground ,snow-storm-1))))

   ;; window dividers
   `(window-divider ((,class (:background ,polar-night-4))))
   `(window-divider-first-pixel ((,class (:background ,polar-night-4))))
   `(window-divider-last-pixel ((,class (:background ,polar-night-4))))

   ;; corfu
   `(corfu-default ((,class (:foreground ,default :background ,black0))))
   `(corfu-current ((,class (:background ,polar-night-1))))
   `(corfu-border ((,class (:background ,comment))))
   `(corfu-annotations ((,class (:foreground ,aurora-purple))))
   `(corfu-bar ((,class (:background ,comment))))

   ;; package.el
   `(package-description ((,class (:foreground ,snow-storm-1))))
   `(package-help-section-name ((,class (:foreground ,frost-2 :weight bold))))
   `(package-name ((,class (:foreground ,frost-2))))
   `(package-status-available ((,class (:foreground ,frost-1))))
   `(package-status-avail-obso ((,class (:foreground ,frost-1 :slant italic))))
   `(package-status-built-in ((,class (:foreground ,frost-3))))
   `(package-status-dependency ((,class (:foreground ,frost-2 :slant italic))))
   `(package-status-disabled ((,class (:foreground ,polar-night-4))))
   `(package-status-external ((,class (:foreground ,aurora-orange :slant italic))))
   `(package-status-held ((,class (:foreground ,snow-storm-1 :weight bold))))
   `(package-status-new ((,class (:foreground ,aurora-green))))
   `(package-status-incompat ((,class (:foreground ,aurora-red))))
   `(package-status-installed ((,class (:foreground ,frost-1 :weight bold))))
   `(package-status-unsigned ((,class (:underline ,aurora-yellow))))

   ;; undo-tree
   `(undo-tree-visualizer-current-face ((,class (:foreground ,frost-2))))
   `(undo-tree-visualizer-default-face ((,class (:foreground ,snow-storm-1))))
   `(undo-tree-visualizer-unmodified-face ((,class (:foreground ,snow-storm-1))))
   `(undo-tree-visualizer-register-face ((,class (:foreground ,frost-3))))

   ;; Whitespace
   `(whitespace-big-indent ((,class (:foreground ,polar-night-4 :background ,background))))
   `(whitespace-empty ((,class (:foreground ,polar-night-4 :background ,background))))
   `(whitespace-hspace ((,class (:foreground ,polar-night-4 :background ,background))))
   `(whitespace-indentation ((,class (:foreground ,polar-night-4 :background unspecified))))
   `(whitespace-line ((,class (:background ,background))))
   `(whitespace-newline ((,class (:foreground ,polar-night-4 :background ,background))))
   `(whitespace-space ((,class (:foreground ,polar-night-4 :background ,background))))
   `(whitespace-space-after-tab ((,class (:foreground ,polar-night-4 :background ,background))))
   `(whitespace-space-before-tab ((,class (:foreground ,polar-night-4 :background ,background))))
   `(whitespace-tab ((,class (:foreground ,polar-night-4 :background ,background))))
   `(whitespace-trailing ((,class (:inherit trailing-whitespace))))
   `(trailing-whitespace ((,class (:background ,aurora-red :foreground ,background :weight bold))))

   ;; VC
   `(vc-conflict-state ((,class (:foreground ,aurora-orange))))
   `(vc-edited-state ((,class (:foreground ,aurora-yellow))))
   `(vc-locally-added-state ((,class (:underline ,aurora-green))))
   `(vc-locked-state ((,class (:foreground ,frost-4))))
   `(vc-missing-state ((,class (:foreground ,aurora-red))))
   `(vc-needs-update-state ((,class (:foreground ,aurora-orange))))
   `(vc-removed-state ((,class (:foreground ,aurora-red))))
   `(vc-state-base ((,class (:foreground ,snow-storm-1))))
   `(vc-up-to-date-state ((,class (:foreground ,frost-2))))

   ;; Enhanced Ruby
   `(enh-ruby-heredoc-delimiter-face ((,class (:foreground ,aurora-green))))
   `(enh-ruby-op-face ((,class (:foreground ,frost-3))))
   `(enh-ruby-regexp-delimiter-face ((,class (:foreground ,aurora-yellow))))
   `(enh-ruby-regexp-face ((,class (:foreground ,aurora-yellow))))
   `(enh-ruby-string-delimiter-face ((,class (:foreground ,aurora-green))))
   `(erm-syn-errline ((,class (:foreground ,aurora-red :underline t))))
   `(erm-syn-warnline ((,class (:foreground ,aurora-yellow :underline t))))

   ;; Markdown
   `(markdown-blockquote-face ((,class (:foreground ,comment))))
   `(markdown-bold-face ((,class (:inherit bold))))
   `(markdown-header-face-1 ((,class (:foreground ,frost-2))))
   `(markdown-header-face-2 ((,class (:foreground ,frost-2))))
   `(markdown-header-face-3 ((,class (:foreground ,frost-2))))
   `(markdown-header-face-4 ((,class (:foreground ,frost-2))))
   `(markdown-header-face-5 ((,class (:foreground ,frost-2))))
   `(markdown-header-face-6 ((,class (:foreground ,frost-2))))
   `(markdown-inline-code-face ((,class (:foreground ,frost-1))))
   `(markdown-italic-face ((,class (:inherit italic))))
   `(markdown-link-face ((,class (:foreground ,frost-2))))
   `(markdown-markup-face ((,class (:foreground ,frost-3))))
   `(markdown-reference-face ((,class (:inherit markdown-link-face))))
   `(markdown-url-face ((,class (:foreground ,snow-storm-1 :underline t))))

   ;; Anzu
   `(anzu-mode-line ((,class (:foreground, frost-2))))
   `(anzu-mode-line-no-match ((,class (:foreground, aurora-red))))

   ;; Avy
   `(avy-lead-face ((,class (:background ,aurora-red :foreground ,snow-storm-2))))
   `(avy-lead-face-0 ((,class (:background ,frost-4 :foreground ,snow-storm-2))))
   `(avy-lead-face-1 ((,class (:background ,polar-night-4 :foreground ,snow-storm-2))))
   `(avy-lead-face-2 ((,class (:background ,aurora-purple :foreground ,snow-storm-2))))

   ;; hl-todo
   `(hl-todo ((,class (:foreground ,aurora-red))))

   ;; diff-hl
   `(diff-hl-change ((,class (:background ,aurora-yellow))))
   `(diff-hl-insert ((,class (:background ,aurora-green))))
   `(diff-hl-delete ((,class (:background ,aurora-red))))

   ;; Evil
   `(evil-ex-info ((,class (:foreground ,frost-2))))
   `(evil-ex-substitute-replacement ((,class (:foreground ,frost-3))))
   `(evil-ex-substitute-matches ((,class (:inherit isearch))))

   ;; flycheck
   `(flycheck-fringe-error ((,class (:foreground ,aurora-red :background nil :weight bold))))
   `(flycheck-fringe-warning ((,class (:background nil :foreground ,aurora-orange :weight bold))))
   `(flycheck-fringe-info ((,class (:background nil :foreground ,frost-2 :weight bold))))
   `(flycheck-warning ((,class (:foreground ,aurora-orange :weight normal))))
   `(flycheck-error ((,class (:foreground ,aurora-red :weight bold))))
   `(flycheck-info ((,class (:foreground ,frost-2))))

   ;; flymake
   `(flymake-warning ((,class (:foreground ,aurora-orange :weight normal))))
   `(flymake-error ((,class (:foreground ,aurora-red :weight bold))))
   `(flymake-note ((,class (:foreground ,frost-2))))

   ;; Magit
   `(magit-branch ((,class (:foreground ,frost-1 :weight bold))))
   `(magit-diff-context-highlight ((,class (:background ,polar-night-1))))
   `(magit-diff-file-header ((,class (:foreground ,frost-2 :box (:color ,frost-2)))))
   `(magit-diffstat-added ((,class (:foreground ,aurora-green))))
   `(magit-diffstat-removed ((,class (:foreground ,aurora-red))))
   `(magit-hash ((,class (:foreground ,frost-2))))
   `(magit-hunk-heading ((,class (:foreground ,frost-3))))
   `(magit-hunk-heading-highlight ((,class (:foreground ,frost-3 :background ,polar-night-1))))
   `(magit-item-highlight ((,class (:foreground ,frost-2 :background ,polar-night-1))))
   `(magit-log-author ((,class (:foreground ,frost-1))))
   `(magit-process-ng ((,class (:foreground ,aurora-yellow :weight bold))))
   `(magit-process-ok ((,class (:foreground ,aurora-green :weight bold))))
   `(magit-section-heading ((,class (:foreground ,frost-1 :weight bold))))
   `(magit-section-highlight ((,class (:background ,polar-night-1))))

   ;; MU4E
   `(mu4e-header-marks-face ((,class (:foreground ,frost-3))))
   `(mu4e-title-face ((,class (:foreground ,frost-2))))
   `(mu4e-header-key-face ((,class (:foreground ,frost-2))))
   `(mu4e-highlight-face ((,class (:highlight))))
   `(mu4e-flagged-face ((,class (:foreground ,aurora-yellow))))
   `(mu4e-unread-face ((,class (:foreground ,aurora-yellow :weight bold))))
   `(mu4e-link-face ((,class (:underline t))))

   ;; Cider
   `(cider-result-overlay-face ((t (:background unspecified))))

   ;; Org
   `(outline-1 ((,class (:foreground ,aurora-orange))))
   `(outline-2 ((,class (:foreground ,aurora-purple))))
   `(outline-3 ((,class (:foreground ,frost-2))))
   `(outline-4 ((,class (:foreground ,aurora-green))))
   `(outline-5 ((,class (:foreground ,aurora-orange))))
   `(outline-6 ((,class (:foreground ,aurora-purple))))
   `(outline-7 ((,class (:foreground ,frost-2))))
   `(outline-8 ((,class (:foreground ,aurora-green))))
   `(org-level-1 ((,class (:inherit outline-1))))
   `(org-level-2 ((,class (:inherit outline-2))))
   `(org-level-3 ((,class (:inherit outline-3))))
   `(org-level-4 ((,class (:inherit outline-4))))
   `(org-level-5 ((,class (:inherit outline-5))))
   `(org-level-6 ((,class (:inherit outline-6))))
   `(org-level-7 ((,class (:inherit outline-7))))
   `(org-level-8 ((,class (:inherit outline-8))))
   `(org-agenda-date ((,class (:foreground ,frost-2 :underline nil))))
   `(org-agenda-date-weekend ((,class (:foreground ,frost-3))))
   `(org-agenda-dimmed-todo-face ((,class (:background ,aurora-yellow))))
   `(org-agenda-done ((,class (:foreground ,aurora-green))))
   `(org-agenda-structure ((,class (:foreground ,frost-3))))
   `(org-block ((,class (:background ,black0 :foreground ,default :extend t))))
   `(org-block-background ((,class (:background ,black0 :extend t))))
   `(org-block-begin-line ((,class (:background ,black0 :foreground ,comment :height 0.8 :extend t))))
   `(org-block-end-line ((,class (:background ,black0 :foreground ,comment :height 0.8 :extend t))))
   `(org-checkbox ((,class (:foreground ,frost-3))))
   `(org-checkbox-statistics-done ((,class (:foreground ,aurora-green))))
   `(org-checkbox-statistics-todo ((,class (:foreground ,aurora-red))))
   `(org-code ((,class (:foreground ,polar-night-6))))
   `(org-column ((,class (:background ,polar-night-3))))
   `(org-column-title ((,class (:inherit org-column :weight bold :underline t))))
   `(org-date ((,class (:foreground ,frost-2))))
   `(org-document-info ((,class (:foreground ,snow-storm-1))))
   `(org-document-info-keyword ((,class (:foreground ,polar-night-4 :weight bold))))
   `(org-document-title ((,class (:foreground ,frost-2 :weight bold))))
   `(org-done ((,class (:foreground ,aurora-green :weight bold))))
   `(org-ellipsis ((,class (:foreground ,polar-night-4))))
   `(org-footnote ((,class (:foreground ,frost-2))))
   `(org-formula ((,class (:foreground ,frost-3))))
   `(org-hide ((,class (:foreground ,black2 :background ,black2))))
   `(org-link ((,class (:underline t))))
   `(org-quote ((,class (:inherit org-block :slant italic))))
   `(org-scheduled ((,class (:foreground ,aurora-green))))
   `(org-scheduled-previously ((,class (:foreground ,aurora-yellow))))
   `(org-scheduled-today ((,class (:foreground ,frost-2))))
   `(org-sexp-date ((,class (:foreground ,frost-1))))
   `(org-special-keyword ((,class (:foreground ,frost-3))))
   `(org-table ((,class (:foreground ,frost-3))))
   `(org-todo ((,class (:foreground ,aurora-red :weight bold))))
   `(org-upcoming-deadline ((,class (:foreground ,aurora-orange))))
   `(org-verbatim ((,class (:foreground ,frost-1))))
   `(org-verse ((,class (:inherit org-block :slant italic))))
   `(org-warning ((,class (:foreground ,aurora-yellow :weight bold))))

   ;; ido
   `(ido-first-match ((,class (:foreground ,frost-2 :weight bold))))
   `(ido-subdir ((,class (:foreground ,frost-3))))
   `(ido-only-match ((,class (:foreground ,frost-2))))

   ;; TeX/LaTeX
   `(font-latex-bold-face ((,class (:inherit bold))))
   `(font-latex-italic-face ((,class (:slant italic))))
   `(font-latex-match-reference-keywords ((,class (:foreground ,frost-3))))
   `(font-latex-match-variable-keywords ((,class (:foreground ,snow-storm-1))))
   `(font-latex-math-face ((,class (:foreground ,frost-2))))
   `(font-latex-script-char-face ((,class (:inherit font-lock-warning-face))))
   `(font-latex-sectioning-0-face ((,class (:foreground ,frost-2 :weight bold))))
   `(font-latex-sectioning-1-face ((,class (:inherit font-latex-sectioning-0-face))))
   `(font-latex-sectioning-2-face ((,class (:inherit font-latex-sectioning-0-face))))
   `(font-latex-sectioning-3-face ((,class (:inherit font-latex-sectioning-0-face))))
   `(font-latex-sectioning-4-face ((,class (:inherit font-latex-sectioning-0-face))))
   `(font-latex-sectioning-5-face ((,class (:inherit font-latex-sectioning-0-face))))
   `(font-latex-string-face ((,class (:inherit font-lock-string-face))))
   `(font-latex-warning-face ((,class (:inherit font-lock-warning-face))))

   ;; perspective
   `(persp-selected-face ((,class (:foreground ,frost-2 :weight bold))))))

(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'efnord)
