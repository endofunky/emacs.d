(let ((base00 "#1d1f21")
      (base01 "#282a2e")
      (base02 "#373b41")
      (base03 "#969896")
      (base04 "#b4b7b4")
      (base05 "#c5c8c6")
      (base06 "#e0e0e0")
      (base07 "#ffffff")
      (base08 "#cc6666")
      (base09 "#de935f")
      (base0A "#f0c674")
      (base0B "#b5bd68")
      (base0C "#8abeb7")
      (base0D "#81a2be")
      (base0E "#b294bb")
      (base0F "#a3685a"))
  (deftheme ef-theme "DOCSTRING for untitled")

  (custom-theme-set-faces
   'ef-theme
;;; Built-in
;;;; basic colors
   `(border ((t (:background ,base03))))
   `(cursor ((t (:background ,base08))))
   `(default ((t (:foreground ,base05 :background ,base00))))
   `(fringe ((t (:background ,base00))))
   `(gui-element ((t (:background ,base01))))
   `(header-line ((t (:foreground ,base0E :background ,base00))))
   `(highlight ((t (:background ,base01))))
   `(link ((t (:foreground ,base0D :underline t))))
   `(link-visited ((t (:foreground ,base0E :underline t))))
   `(minibuffer-prompt ((t (:foreground ,base0D))))
   `(region ((t (:background ,base02 :distant-foreground ,base05))))
   `(secondary-selection ((t (:background ,base03 :distant-foreground ,base05))))
   `(trailing-whitespace ((t (:foreground nil :background ,base08))))
   `(vertical-border ((t (:foreground ,base01))))
   `(widget-button ((t (:underline t))))
   `(widget-field ((t (:background ,base03 :box (:line-width 1 :color ,base06)))))

   `(error ((t (:foreground ,base08 :weight bold))))
   `(warning ((t (:foreground ,base09 :weight bold))))
   `(success ((t (:foreground ,base0B :weight bold))))
   `(shadow ((t (:foreground ,base03))))

;;;; compilation
   `(compilation-column-number ((t (:foreground ,base0A))))
   `(compilation-line-number ((t (:foreground ,base0A))))
   `(compilation-message-face ((t (:foreground ,base0D))))
   `(compilation-mode-line-exit ((t (:foreground ,base0B))))
   `(compilation-mode-line-fail ((t (:foreground ,base08))))
   `(compilation-mode-line-run ((t (:foreground ,base0D))))

;;;; custom
   `(custom-variable-tag ((t (:foreground ,base0D))))
   `(custom-group-tag ((t (:foreground ,base0D))))
   `(custom-state ((t (:foreground ,base0B))))

;;;; font-lock
   `(font-lock-builtin-face ((t (:foreground ,base0C))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,base03))))
   `(font-lock-comment-face ((t (:foreground ,base03))))
   `(font-lock-constant-face ((t (:foreground ,base09))))
   `(font-lock-doc-face ((t (:foreground ,base03))))
   `(font-lock-doc-string-face ((t (:foreground ,base03))))
   `(font-lock-function-name-face ((t (:foreground ,base0D))))
   `(font-lock-keyword-face ((t (:foreground ,base0E))))
   `(font-lock-negation-char-face ((t (:foreground ,base0B))))
   `(font-lock-preprocessor-face ((t (:foreground ,base0D))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,base0A))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,base0E))))
   `(font-lock-string-face ((t (:foreground ,base0B))))
   `(font-lock-type-face ((t (:foreground ,base0A))))
   `(font-lock-variable-name-face ((t (:foreground ,base08))))
   `(font-lock-warning-face ((t (:foreground ,base08))))

;;;; isearch
   `(match ((t (:foreground ,base0D :background ,base01 :inverse-video t))))
   `(isearch ((t (:foreground ,base0A :background ,base01 :inverse-video t))))
   `(lazy-highlight ((t (:foreground ,base0C :background ,base01 :inverse-video t))))
   `(isearch-lazy-highlight-face ((t (:inherit lazy-highlight)))) ;; was replaced with 'lazy-highlight in emacs 22
   `(isearch-fail ((t (:background ,base01 :inverse-video t :inherit font-lock-warning-face))))

;;;; line-numbers
   `(line-number ((t (:foreground ,base02 :background ,base00))))
   `(line-number-current-line ((t (:background ,base01))))

;;;; mode-line
   `(mode-line ((t (:foreground ,base04 :background ,base02 :box nil))))
   `(mode-line-buffer-id ((t (:foreground ,base0B :background nil))))
   `(mode-line-emphasis ((t (:foreground ,base06 :slant italic))))
   `(mode-line-highlight ((t (:foreground ,base0E :box nil :weight bold))))
   `(mode-line-inactive ((t (:foreground ,base03 :background ,base01 :box nil))))

;;; Third-party

;;;; anzu-mode
   `(anzu-mode-line ((t (:foreground ,base0E))))

;;;; auctex
   `(font-latex-bold-face ((t (:foreground ,base0B))))
   `(font-latex-doctex-documentation-face ((t (:background ,base03))))
   `(font-latex-italic-face ((t (:foreground ,base0B))))
   `(font-latex-math-face ((t (:foreground ,base09))))
   `(font-latex-sectioning-0-face ((t (:foreground ,base0A))))
   `(font-latex-sectioning-1-face ((t (:foreground ,base0A))))
   `(font-latex-sectioning-2-face ((t (:foreground ,base0A))))
   `(font-latex-sectioning-3-face ((t (:foreground ,base0A))))
   `(font-latex-sectioning-4-face ((t (:foreground ,base0A))))
   `(font-latex-sectioning-5-face ((t (:foreground ,base0A))))
   `(font-latex-sedate-face ((t (:foreground ,base0C))))
   `(font-latex-string-face ((t (:foreground ,base0A))))
   `(font-latex-verbatim-face ((t (:foreground ,base09))))
   `(font-latex-warning-face ((t (:foreground ,base08))))

   `(TeX-error-description-error ((t (:inherit error))))
   `(TeX-error-description-tex-said ((t (:inherit font-lock-function-name-face))))
   `(TeX-error-description-warning ((t (:inherit warning))))

;;;; centaur-tabs
   `(centaur-tabs-default ((t (:background ,base01 :foreground ,base01))))
   `(centaur-tabs-selected ((t (:background ,base00 :foreground ,base06))))
   `(centaur-tabs-unselected ((t (:background ,base01 :foreground ,base05))))
   `(centaur-tabs-selected-modified ((t (:background ,base00 :foreground ,base0D))))
   `(centaur-tabs-unselected-modified ((t (:background ,base01 :foreground ,base0D))))
   `(centaur-tabs-active-bar-face ((t (:background ,base0D))))
   `(centaur-tabs-modified-marker-selected ((t (:inherit 'centaur-tabs-selected :foreground ,base0D))))
   `(centaur-tabs-modified-marker-unselected ((t (:inherit 'centaur-tabs-unselected :foreground ,base0D))))

;;;; circe-mode
   `(circe-fool-face ((t (:foreground ,base02))))
   `(circe-my-message-face ((t (:foreground ,base0B))))
   `(circe-highlight-nick-face ((t (:foreground ,base0A))))
   `(circe-originator-face ((t (:foreground ,base0E))))
   `(circe-prompt-face ((t (:foreground ,base0D))))
   `(circe-server-face ((t (:foreground ,base03))))

;;;; avy
   `(avy-lead-face-0 ((t (:foreground ,base00 :background ,base0C))))
   `(avy-lead-face-1 ((t (:foreground ,base00 :background ,base05))))
   `(avy-lead-face-2 ((t (:foreground ,base00 :background ,base0E))))
   `(avy-lead-face ((t (:foreground ,base00 :background ,base09))))
   `(avy-background-face ((t (:foreground ,base03))))
   `(avy-goto-char-timer-face ((t (:inherit highlight))))

;;;; clojure-mode
   `(clojure-keyword-face ((t (:foreground ,base0E))))

;;;; company-mode
   `(company-tooltip ((t (:inherit tooltip))))
   `(company-scrollbar-bg ((t (:background ,base07))))
   `(company-scrollbar-fg ((t (:background ,base04))))
   `(company-tooltip-annotation ((t (:foreground ,base08))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))
   `(company-tooltip-selection ((t (:background ,base02 :inherit font-lock-function-name-face))))
   `(company-tooltip-search ((t (:inherit match))))
   `(company-tooltip-search-selection ((t (:inherit match))))
   `(company-preview-common ((t (:inherit secondary-selection))))
   `(company-preview ((t (:foreground ,base04))))
   `(company-preview-search ((t (:inherit match))))
   `(company-echo-common ((t (:inherit secondary-selection))))

;;;; cperl-mode
   `(cperl-array-face ((t (:weight bold :inherit font-lock-variable-name-face))))
   `(cperl-hash-face ((t (:weight bold :slant italic :inherit font-lock-variable-name-face))))
   `(cperl-nonoverridable-face ((t (:inherit font-lock-builtin-face))))

;;;; cscope-minor-mode
   `(cscope-file-face ((t (:foreground ,base0B))))
   `(cscope-function-face ((t (:foreground ,base0D))))
   `(cscope-line-number-face ((t (:foreground ,base0A))))
   `(cscope-mouse-face ((t (:foreground ,base04 :background ,base01))))
   `(cscope-separator-face ((t (:foreground ,base08 :overline t :underline t :weight bold))))

;;;; csv-mode
   `(csv-separator-face ((t (:foreground ,base09))))

;;;; diff-hl-mode
   `(diff-hl-change ((t (:foreground ,base0E))))
   `(diff-hl-delete ((t (:foreground ,base08))))
   `(diff-hl-insert ((t (:foreground ,base0B))))

;;;; diff-mode
   `(diff-added ((t (:foreground ,base0B))))
   `(diff-changed ((t (:foreground ,base0E))))
   `(diff-removed ((t (:foreground ,base08))))
   `(diff-header ((t (:background ,base01))))
   `(diff-file-header ((t (:background ,base02))))
   `(diff-hunk-header ((t (:foreground ,base0E :background ,base01))))

;;;; dired
   `(dired-filetype-plain ((t (:foreground ,base05 :background ,base00))))

;;;; dired+
   `(diredp-compressed-file-suffix ((t (:foreground ,base0D))))
   `(diredp-dir-heading ((t (:foreground nil :background nil :inherit heading))))
   `(diredp-dir-priv ((t (:foreground ,base0C :background nil))))
   `(diredp-exec-priv ((t (:foreground ,base0D :background nil))))
   `(diredp-executable-tag ((t (:foreground ,base08 :background nil))))
   `(diredp-file-name ((t (:foreground ,base0A))))
   `(diredp-file-suffix ((t (:foreground ,base0B))))
   `(diredp-flag-mark-line ((t (:background nil :inherit highlight))))
   `(diredp-ignored-file-name ((t (:foreground ,base04))))
   `(diredp-link-priv ((t (:foreground ,base0E :background nil))))
   `(diredp-mode-line-flagged ((t (:foreground ,base08))))
   `(diredp-mode-line-marked ((t (:foreground ,base0B))))
   `(diredp-no-priv ((t (:background nil))))
   `(diredp-number ((t (:foreground ,base0A))))
   `(diredp-other-priv ((t (:foreground ,base0E :background nil))))
   `(diredp-rare-priv ((t (:foreground ,base08 :background nil))))
   `(diredp-read-priv ((t (:foreground ,base0B :background nil))))
   `(diredp-symlink ((t (:foreground ,base0E))))
   `(diredp-write-priv ((t (:foreground ,base0A :background nil))))

;;;; diredfl
   `(diredfl-autofile-name ((t (:foreground ,base0E))))
   `(diredfl-compressed-file-name ((t (:foreground ,base0A))))
   `(diredfl-compressed-file-suffix ((t (:foreground ,base0D))))
   `(diredfl-date-time ((t (:foreground ,base0C :weight light))))
   `(diredfl-deletion ((t (:foreground nil :background ,base08))))
   `(diredfl-deletion-file-name ((t (:foreground ,base00 :background ,base08 :weight bold))))
   `(diredfl-dir-heading ((t (:foreground nil :background nil :inherit heading :weight bold))))
   `(diredfl-dir-name ((t (:foreground ,base0D))))
   `(diredfl-dir-priv ((t (:foreground ,base0D :background nil))))
   `(diredfl-exec-priv ((t (:foreground ,base08 :background nil))))
   `(diredfl-executable-tag ((t (:foreground ,base08 :background nil))))
   `(diredfl-file-name ((t (:foreground ,base0A))))
   `(diredfl-file-suffix ((t (:foreground ,base0B))))
   `(diredfl-flag-mark ((t (:foreground ,base09 :weight bold))))
   `(diredfl-flag-mark-line ((t (:background nil :inherit highlight))))
   `(diredfl-ignored-file-name ((t (:foreground ,base04))))
   `(diredfl-link-priv ((t (:foreground ,base0E :background nil))))
   `(diredfl-no-priv ((t (:background nil))))
   `(diredfl-number ((t (:foreground ,base0A))))
   `(diredfl-other-priv ((t (:foreground ,base0E :background nil))))
   `(diredfl-rare-priv ((t (:foreground ,base0F :background nil))))
   `(diredfl-read-priv ((t (:foreground ,base0B :background nil))))
   `(diredfl-symlink ((t (:foreground ,base0E))))
   `(diredfl-tagged-autofile-name ((t (:foreground ,base05))))
   `(diredfl-write-priv ((t (:foreground ,base0A :background nil))))

;;;; doom-modeline
   `(doom-modeline-eldoc-bar ((t (:background ,base0B))))
   `(doom-modeline-inactive-bar ((t (:background nil)))) ; transparent
   `(doom-modeline-bar ((t (:background ,base0D))))

;;;; ediff-mode
   `(ediff-even-diff-A ((t (:inverse-video t))))
   `(ediff-even-diff-B ((t (:inverse-video t))))
   `(ediff-even-diff-C ((t (:inverse-video t))))
   `(ediff-odd-diff-A ((t (:foreground ,base04 :inverse-video t))))
   `(ediff-odd-diff-B ((t (:foreground ,base04 :inverse-video t))))
   `(ediff-odd-diff-C ((t (:foreground ,base04 :inverse-video t))))

;;;; eldoc-mode
   `(eldoc-highlight-function-argument ((t (:foreground ,base0B :weight bold))))

;;;; erc
   `(erc-direct-msg-face ((t (:foreground ,base09))))
   `(erc-error-face ((t (:foreground ,base08))))
   `(erc-header-face ((t (:foreground ,base06 :background ,base04))))
   `(erc-input-face ((t (:foreground ,base0B))))
   `(erc-keyword-face ((t (:foreground ,base0A))))
   `(erc-current-nick-face ((t (:foreground ,base0B))))
   `(erc-my-nick-face ((t (:foreground ,base0B))))
   `(erc-nick-default-face ((t (:foreground ,base0E :weight normal))))
   `(erc-nick-msg-face ((t (:foreground ,base0A :weight normal))))
   `(erc-notice-face ((t (:foreground ,base04))))
   `(erc-pal-face ((t (:foreground ,base09))))
   `(erc-prompt-face ((t (:foreground ,base0D))))
   `(erc-timestamp-face ((t (:foreground ,base0C))))

;;;; eshell
   `(eshell-ls-archive ((t (:foreground ,base08))))
   `(eshell-ls-backup ((t (:foreground ,base0F))))
   `(eshell-ls-clutter ((t (:foreground ,base09))))
   `(eshell-ls-directory ((t (:foreground ,base0D))))
   `(eshell-ls-executable ((t (:foreground ,base0B))))
   `(eshell-ls-missing ((t (:foreground ,base08))))
   `(eshell-ls-product ((t (:foreground ,base0F))))
   `(eshell-ls-readonly ((t (:foreground ,base06))))
   `(eshell-ls-special ((t (:foreground ,base0E))))
   `(eshell-ls-symlink ((t (:foreground ,base0C))))
   `(eshell-ls-unreadable ((t (:foreground ,base04))))
   `(eshell-prompt ((t (:foreground ,base05))))

;;;; evil-mode
   `(evil-search-highlight-persist-highlight-face ((t (:background ,base01 :inverse-video t :inherit font-lock-warning-face))))

;;;; fic-mode
   `(fic-author-face ((t (:foreground ,base09 :underline t))))
   `(fic-face ((t (:foreground ,base08 :weight bold))))

;;;; flycheck-mode
   `(flycheck-error ((t (:underline (:style line :underline ,base08)))))
   `(flycheck-info ((t (:underline (:style line :underline ,base0B)))))
   `(flycheck-warning ((t (:underline (:style line :underline ,base09)))))

;;;; flymake-mode
   `(flymake-warnline ((t (:background ,base01 :underline ,base09))))
   `(flymake-errline ((t (:background ,base01 :underline ,base08))))
   `(flymake-warning ((t (:background ,base01 :underline ,base09))))
   `(flymake-error ((t (:background ,base01 :underline ,base08))))

;;;; flyspell-mode
   `(flyspell-duplicate ((t (:underline (:style wave :color ,base09)))))
   `(flyspell-incorrect ((t (:underline (:style wave :color ,base08)))))

;;;; git-gutter-mode
   `(git-gutter:added ((t (:foreground ,base0B))))
   `(git-gutter:deleted ((t (:foreground ,base08))))
   `(git-gutter:modified ((t (:foreground ,base0E))))
   `(git-gutter:separator ((t (:foreground ,base0C))))
   `(git-gutter:unchanged ((t (:foreground ,base0A :inverse-video t))))

;;;; git-gutter+-mode
   '(git-gutter+-added                            :foreground ,base0B)
   '(git-gutter+-deleted                          :foreground ,base08)
   '(git-gutter+-modified                         :foreground ,base0E)
   '(git-gutter+-unchanged                        :foreground ,base0A :inverse-video t)

;;;; git-gutter-fringe
   `(git-gutter-fr:added ((t (:foreground ,base0B))))
   `(git-gutter-fr:deleted ((t (:foreground ,base08))))
   `(git-gutter-fr:modified ((t (:foreground ,base0E))))

;;;; gnus
   `(gnus-cite-1 ((t (:foreground nil :inherit outline-1))))
   `(gnus-cite-2 ((t (:foreground nil :inherit outline-2))))
   `(gnus-cite-3 ((t (:foreground nil :inherit outline-3))))
   `(gnus-cite-4 ((t (:foreground nil :inherit outline-4))))
   `(gnus-cite-5 ((t (:foreground nil :inherit outline-5))))
   `(gnus-cite-6 ((t (:foreground nil :inherit outline-6))))
   `(gnus-cite-7 ((t (:foreground nil :inherit outline-7))))
   `(gnus-cite-8 ((t (:foreground nil :inherit outline-8))))
   ;; there are several more -cite- faces...
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-header-from ((t (:foreground ,base09 :weight bold :inherit message-header-other-face))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-button ((t (:foreground nil :inherit link))))
   `(gnus-signature ((t (:inherit font-lock-comment-face))))

   `(gnus-summary-normal-unread ((t (:foreground ,base0D :weight normal))))
   `(gnus-summary-normal-read ((t (:foreground ,base06 :weight normal))))
   `(gnus-summary-normal-ancient ((t (:foreground ,base0C :weight normal))))
   `(gnus-summary-normal-ticked ((t (:foreground ,base09 :weight normal))))
   `(gnus-summary-low-unread ((t (:foreground ,base04 :weight normal))))
   `(gnus-summary-low-read ((t (:foreground ,base04 :weight normal))))
   `(gnus-summary-low-ancient ((t (:foreground ,base04 :weight normal))))
   `(gnus-summary-high-unread ((t (:foreground ,base0A :weight normal))))
   `(gnus-summary-high-read ((t (:foreground ,base0B :weight normal))))
   `(gnus-summary-high-ancient ((t (:foreground ,base0B :weight normal))))
   `(gnus-summary-high-ticked ((t (:foreground ,base09 :weight normal))))
   `(gnus-summary-cancelled ((t (:foreground ,base08 :background nil :weight normal))))

   `(gnus-group-mail-low ((t (:foreground ,base04))))
   `(gnus-group-mail-low-empty ((t (:foreground ,base04))))
   `(gnus-group-mail-1 ((t (:foreground nil :weight normal :inherit outline-1))))
   `(gnus-group-mail-2 ((t (:foreground nil :weight normal :inherit outline-2))))
   `(gnus-group-mail-3 ((t (:foreground nil :weight normal :inherit outline-3))))
   `(gnus-group-mail-4 ((t (:foreground nil :weight normal :inherit outline-4))))
   `(gnus-group-mail-5 ((t (:foreground nil :weight normal :inherit outline-5))))
   `(gnus-group-mail-6 ((t (:foreground nil :weight normal :inherit outline-6))))
   `(gnus-group-mail-1-empty ((t (:foreground ,base04 :inherit gnus-group-mail-1))))
   `(gnus-group-mail-2-empty ((t (:foreground ,base04 :inherit gnus-group-mail-2))))
   `(gnus-group-mail-3-empty ((t (:foreground ,base04 :inherit gnus-group-mail-3))))
   `(gnus-group-mail-4-empty ((t (:foreground ,base04 :inherit gnus-group-mail-4))))
   `(gnus-group-mail-5-empty ((t (:foreground ,base04 :inherit gnus-group-mail-5))))
   `(gnus-group-mail-6-empty ((t (:foreground ,base04 :inherit gnus-group-mail-6))))
   `(gnus-group-news-1 ((t (:foreground nil :weight normal :inherit outline-5))))
   `(gnus-group-news-2 ((t (:foreground nil :weight normal :inherit outline-6))))
   `(gnus-group-news-3 ((t (:foreground nil :weight normal :inherit outline-7))))
   `(gnus-group-news-4 ((t (:foreground nil :weight normal :inherit outline-8))))
   `(gnus-group-news-5 ((t (:foreground nil :weight normal :inherit outline-1))))
   `(gnus-group-news-6 ((t (:foreground nil :weight normal :inherit outline-2))))
   `(gnus-group-news-1-empty ((t (:foreground ,base04 :inherit gnus-group-news-1))))
   `(gnus-group-news-2-empty ((t (:foreground ,base04 :inherit gnus-group-news-2))))
   `(gnus-group-news-3-empty ((t (:foreground ,base04 :inherit gnus-group-news-3))))
   `(gnus-group-news-4-empty ((t (:foreground ,base04 :inherit gnus-group-news-4))))
   `(gnus-group-news-5-empty ((t (:foreground ,base04 :inherit gnus-group-news-5))))
   `(gnus-group-news-6-empty ((t (:foreground ,base04 :inherit gnus-group-news-6))))

;;;; go-guru
   `(go-guru-hl-identifier-face ((t (:background ,base02))))

;;;; grep
   `(grep-context-face ((t (:foreground ,base04))))
   `(grep-error-face ((t (:foreground ,base08 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,base0D))))
   `(grep-match-face ((t (:foreground nil :background nil :inherit match))))

;;;; helm
   `(helm-M-x-key ((t (:foreground ,base0C))))
   `(helm-action ((t (:foreground ,base05))))
   `(helm-buffer-directory ((t (:foreground ,base04 :background nil :weight bold))))
   `(helm-buffer-file ((t (:foreground ,base0C))))
   `(helm-buffer-not-saved ((t (:foreground ,base08))))
   `(helm-buffer-process ((t (:foreground ,base03))))
   `(helm-buffer-saved-out ((t (:foreground ,base0F))))
   `(helm-buffer-size ((t (:foreground ,base09))))
   `(helm-candidate-number ((t (:foreground ,base00 :background ,base09))))
   `(helm-ff-directory ((t (:inherit dired-directory))))
   `(helm-ff-dotted-directory ((t (:inherit dired-ignored))))
   `(helm-ff-executable ((t (:foreground ,base0B))))
   `(helm-ff-file ((t (:inherit default))))
   `(helm-ff-invalid-symlink ((t (:inherit dired-warning))))
   `(helm-ff-prefix ((t (:foreground nil :background nil))))
   `(helm-ff-symlink ((t (:inherit dired-symlink))))
   `(helm-ff-suid ((t (:foreground ,base08))))
   `(helm-ff-dotted-symlink-directory ((t (:foreground ,base09 :background ,base03))))
   `(helm-ff-denied ((t (:foreground ,base08 :background ,base03))))
                                        ;     '(helm-ff-truename) ;; already inherited
                                        ;     '(helm-ff-dirs) ;; already inherited
   `(helm-ff-socket ((t (:foreground ,base0E))))
   `(helm-ff-pipe ((t (:foreground ,base0A :background ,base03))))
   `(helm-ff-file-extension ((t (:foreground ,base03))))
   `(helm-ff-backup-file ((t (:inherit dired-ignored))))

   `(helm-grep-cmd-line ((t (:foreground ,base0B))))
   `(helm-grep-file ((t (:foreground ,base0C))))
   `(helm-grep-finish ((t (:foreground ,base00 :background ,base09))))
   `(helm-grep-lineno ((t (:foreground ,base03))))
   `(helm-grep-match ((t (:foreground ,base0A))))
   `(helm-grep-running ((t (:foreground ,base09))))
   `(helm-header ((t (:foreground ,base0A :background ,base00 :underline nil))))
   `(helm-match ((t (:foreground ,base0A))))
   `(helm-moccur-buffer ((t (:foreground ,base0C))))
   `(helm-selection ((t (:foreground nil :background ,base02 :underline nil))))
   `(helm-selection-line ((t (:foreground nil :background ,base02))))
   `(helm-separator ((t (:foreground ,base02))))
   `(helm-source-header ((t (:foreground ,base05 :background ,base01 :weight bold))))
   `(helm-visible-mark ((t (:foreground ,base00 :background ,base0B))))

;;;; highlight-indentation minor mode
   `(highlight-indentation-face ((t (:background ,base01))))

;;;; highlight-thing mode
   `(highlight-thing ((t (:inherit highlight))))

;;;; hl-line-mode
   `(hl-line ((t (:background ,base01 :extend t))))
   `(col-highlight ((t (:background ,base01))))

;;;; hl-sexp-mode
   `(hl-sexp-face ((t (:background ,base03))))

;;;; hydra
   `(hydra-face-red ((t (:foreground ,base09))))
   `(hydra-face-blue ((t (:foreground ,base0D))))

;;;; ido-mode
   `(ido-subdir ((t (:foreground ,base04))))
   `(ido-first-match ((t (:foreground ,base09 :weight bold))))
   `(ido-only-match ((t (:foreground ,base08 :weight bold))))
   `(ido-indicator ((t (:foreground ,base08 :background ,base01))))
   `(ido-virtual ((t (:foreground ,base04))))

;;;; idris-mode
   `(idris-semantic-bound-face ((t (:inherit font-lock-variable-name-face))))
   `(idris-semantic-data-face ((t (:inherit font-lock-string-face))))
   `(idris-semantic-function-face ((t (:inherit font-lock-function-name-face))))
   `(idris-semantic-namespace-face ((t (nil))))
   `(idris-semantic-postulate-face ((t (:inherit font-lock-builtin-face))))
   `(idris-semantic-type-face ((t (:inherit font-lock-type-face))))
   `(idris-active-term-face ((t (:inherit highlight))))
   `(idris-colon-face ((t (:inherit font-lock-keyword-face))))
   `(idris-equals-face ((t (:inherit font-lock-keyword-face))))
   `(idris-operator-face ((t (:inherit font-lock-keyword-face))))

;;;; imenu-list
   `(imenu-list-entry-face-0 ((t (:foreground ,base0A))))
   `(imenu-list-entry-face-1 ((t (:foreground ,base0B))))
   `(imenu-list-entry-face-2 ((t (:foreground ,base0D))))
   `(imenu-list-entry-face-3 ((t (:foreground ,base0F))))

;;;; ivy-mode
   `(ivy-current-match ((t (:foreground ,base09 :background ,base01))))
   `(ivy-minibuffer-match-face-1 ((t (:foreground ,base0E))))
   `(ivy-minibuffer-match-face-2 ((t (:foreground ,base0D))))
   `(ivy-minibuffer-match-face-3 ((t (:foreground ,base0C))))
   `(ivy-minibuffer-match-face-4 ((t (:foreground ,base0B))))
   `(ivy-confirm-face ((t (:foreground ,base0B))))
   `(ivy-match-required-face ((t (:foreground ,base08))))
   `(ivy-virtual ((t (:foreground ,base04))))
   `(ivy-action ((t (:foreground ,base0D))))

;;;; jabber
   `(jabber-chat-prompt-local ((t (:foreground ,base0A))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,base09))))
   `(jabber-chat-prompt-system ((t (:foreground ,base0A :weight bold))))
   `(jabber-chat-text-local ((t (:foreground ,base0A))))
   `(jabber-chat-text-foreign ((t (:foreground ,base09))))
   `(jabber-chat-text-error ((t (:foreground ,base08))))

   `(jabber-roster-user-online ((t (:foreground ,base0B))))
   `(jabber-roster-user-xa ((t (:foreground ,base04))))
   `(jabber-roster-user-dnd ((t (:foreground ,base0A))))
   `(jabber-roster-user-away ((t (:foreground ,base09))))
   `(jabber-roster-user-chatty ((t (:foreground ,base0E))))
   `(jabber-roster-user-error ((t (:foreground ,base08))))
   `(jabber-roster-user-offline ((t (:foreground ,base04))))

   `(jabber-rare-time-face ((t (:foreground ,base04))))
   `(jabber-activity-face ((t (:foreground ,base0E))))
   `(jabber-activity-personal-face ((t (:foreground ,base0C))))

;;;; js2-mode
   `(js2-warning-face ((t (:underline ,base09))))
   `(js2-error-face ((t (:foreground nil :underline ,base08))))
   `(js2-external-variable-face ((t (:foreground ,base0E))))
   `(js2-function-param-face ((t (:foreground ,base0D))))
   `(js2-instance-member-face ((t (:foreground ,base0D))))
   `(js2-private-function-call-face ((t (:foreground ,base08))))

;;;; js3-mode
   `(js3-warning-face ((t (:underline ,base09))))
   `(js3-error-face ((t (:foreground nil :underline ,base08))))
   `(js3-external-variable-face ((t (:foreground ,base0E))))
   `(js3-function-param-face ((t (:foreground ,base0D))))
   `(js3-jsdoc-tag-face ((t (:foreground ,base09))))
   `(js3-jsdoc-type-face ((t (:foreground ,base0C))))
   `(js3-jsdoc-value-face ((t (:foreground ,base0A))))
   `(js3-jsdoc-html-tag-name-face ((t (:foreground ,base0D))))
   `(js3-jsdoc-html-tag-delimiter-face ((t (:foreground ,base0B))))
   `(js3-instance-member-face ((t (:foreground ,base0D))))
   `(js3-private-function-call-face ((t (:foreground ,base08))))

;;;; linum-mode
   `(linum ((t (:foreground ,base03 :background ,base04))))

;;;; lsp-ui-doc
   `(lsp-ui-doc-header ((t (:inherit org-document-title))))
   `(lsp-ui-doc-background ((t (:background ,base01))))

;;;; lui-mode
   `(lui-button-face ((t (:foreground ,base0D))))
   `(lui-highlight-face ((t (:background ,base01))))
   `(lui-time-stamp-face ((t (:foreground ,base0C))))

;;;; macrostep
   `(macrostep-expansion-highlight-face ((t (:background ,base01))))

;;;; magit
   `(magit-blame-culprit ((t (:background ,base01))))
   `(magit-blame-heading ((t (:background ,base01 :foreground ,base05))))
   `(magit-branch ((t (:foreground ,base04 :weight bold))))
   `(magit-branch-current ((t (:foreground ,base0C :weight bold :box t))))
   `(magit-branch-local ((t (:foreground ,base0C))))
   `(magit-branch-remote ((t (:foreground ,base0B))))
   `(magit-cherry-equivalent ((t (:foreground ,base0E))))
   `(magit-cherry-unmatched ((t (:foreground ,base0C))))
   `(magit-diff-added ((t (:foreground ,base0B))))
   `(magit-diff-added-highlight ((t (:background ,base01 :foreground ,base0B))))
   `(magit-diff-context-highlight ((t (:background ,base01))))
   `(magit-diff-file-header ((t (:background nil :foreground ,base05))))
   `(magit-diff-removed ((t (:foreground ,base08))))
   `(magit-diff-removed-highlight ((t (:background ,base01 :foreground ,base08))))
   `(magit-hash ((t (:background nil :foreground ,base05))))
   `(magit-header-line ((t (:background ,base02 :foreground ,base05 :weight bold))))
   `(magit-hunk-heading ((t (:background ,base03))))
   `(magit-hunk-heading-highlight ((t (:background ,base03))))
   `(magit-diff-hunk-heading ((t (:background ,base01))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,base01))))
   `(magit-item-highlight ((t (:background ,base01))))
   `(magit-log-author ((t (:foreground ,base0D))))
   `(magit-process-ng ((t (:foreground ,base08 :inherit magit-section-heading))))
   `(magit-process-ok ((t (:foreground ,base0B :inherit magit-section-heading))))
   `(magit-reflog-amend ((t (:foreground ,base0E))))
   `(magit-reflog-checkout ((t (:foreground ,base0D))))
   `(magit-reflog-cherry-pick ((t (:foreground ,base0B))))
   `(magit-reflog-commit ((t (:foreground ,base0B))))
   `(magit-reflog-merge ((t (:foreground ,base0B))))
   `(magit-reflog-other ((t (:foreground ,base0C))))
   `(magit-reflog-rebase ((t (:foreground ,base0E))))
   `(magit-reflog-remote ((t (:foreground ,base0C))))
   `(magit-reflog-reset ((t (:foreground ,base08))))
   `(magit-section-heading ((t (:foreground ,base0A :weight medium))))
   `(magit-section-highlight ((t (:background ,base01))))
   `(magit-signature-bad ((t (:foreground ,base08 :weight bold))))
   `(magit-signature-error ((t (:foreground ,base08))))
   `(magit-signature-expired ((t (:foreground ,base09))))
   `(magit-signature-good ((t (:foreground ,base0B))))
   `(magit-signature-revoked ((t (:foreground ,base0E))))
   `(magit-signature-untrusted ((t (:foreground ,base0C))))
   `(magit-tag ((t (:foreground ,base05))))
;;;; mark-multiple
   `(mm/master-face ((t (:foreground nil :background nil :inherit region))))
   `(mm/mirror-face ((t (:foreground nil :background nil :inherit region))))

;;;; markdown-mode
   `(markdown-url-face ((t (:inherit link))))
   `(markdown-link-face ((t (:foreground ,base0D :underline t))))

;;;; message-mode
   `(message-header-other ((t (:foreground nil :background nil :weight normal))))
   `(message-header-subject ((t (:foreground ,base0A :weight bold :inherit message-header-other))))
   `(message-header-to ((t (:foreground ,base09 :weight bold :inherit message-header-other))))
   `(message-header-cc ((t (:foreground nil :inherit message-header-to))))
   `(message-header-name ((t (:foreground ,base0D :background nil))))
   `(message-header-newsgroups ((t (:foreground ,base0C :background nil :slant normal))))
   `(message-separator ((t (:foreground ,base0E))))

;;;; mic-paren
   `(paren-face-match ((t (:foreground nil :background nil :inherit show-paren-match))))
   `(paren-face-mismatch ((t (:foreground nil :background nil :inherit show-paren-mismatch))))
   `(paren-face-no-match ((t (:foreground nil :background nil :inherit show-paren-mismatch))))

;;;; mmm-mode
   `(mmm-code-submode-face ((t (:background ,base03))))
   `(mmm-comment-submode-face ((t (:inherit font-lock-comment-face))))
   `(mmm-output-submode-face ((t (:background ,base03))))

;;;; nxml-mode
   `(nxml-name-face ((t (:foreground unspecified :inherit font-lock-constant-face))))
   `(nxml-attribute-local-name-face ((t (:foreground unspecified :inherit font-lock-variable-name-face))))
   `(nxml-ref-face ((t (:foreground unspecified :inherit font-lock-preprocessor-face))))
   `(nxml-delimiter-face ((t (:foreground unspecified :inherit font-lock-keyword-face))))
   `(nxml-delimited-data-face ((t (:foreground unspecified :inherit font-lock-string-face))))
   `(rng-error-face ((t (:underline ,base08))))

;;;; org-mode
   `(org-agenda-structure ((t (:foreground ,base0E))))
   `(org-agenda-date ((t (:foreground ,base0D :underline nil))))
   `(org-agenda-done ((t (:foreground ,base0B))))
   `(org-agenda-dimmed-todo-face ((t (:foreground ,base04))))
   `(org-block ((t (:foreground ,base04))))
   `(org-block-begin-line ((t (:foreground ,base03 :background ,base01))))
   `(org-code ((t (:foreground ,base0A))))
   `(org-column ((t (:background ,base01))))
   `(org-column-title ((t (:weight bold :underline t :inherit org-column))))
   `(org-date ((t (:foreground ,base0E :underline t))))
   `(org-document-info ((t (:foreground ,base0C))))
   `(org-document-info-keyword ((t (:foreground ,base0B))))
   `(org-document-title ((t (:foreground ,base09 :weight medium :height 1.0))))
   `(org-done ((t (:foreground ,base0B :background ,base01))))
   `(org-ellipsis ((t (:foreground ,base04))))
   `(org-footnote ((t (:foreground ,base0C))))
   `(org-formula ((t (:foreground ,base08))))
   `(org-hide ((t (:foreground ,base03))))
   `(org-link ((t (:foreground ,base0D))))
   `(org-scheduled ((t (:foreground ,base0B))))
   `(org-scheduled-previously ((t (:foreground ,base09))))
   `(org-scheduled-today ((t (:foreground ,base0B))))
   `(org-special-keyword ((t (:foreground ,base09))))
   `(org-table ((t (:foreground ,base0E))))
   `(org-todo ((t (:foreground ,base08 :background ,base01))))
   `(org-upcoming-deadline ((t (:foreground ,base09))))
   `(org-warning ((t (:foreground ,base08 :weight bold))))

;;;; paren-face-mode
   `(paren-face ((t (:foreground ,base04 :background nil))))

;;;; perspective-mode
   `(persp-selected-face ((t (:foreground ,base0C))))

;;;; popup
   `(popup-face ((t (:foreground ,base05 :background ,base02))))
   `(popup-isearch-match ((t (:foreground ,base00 :background ,base0B))))
   `(popup-scroll-bar-background-face ((t (:background ,base03))))
   `(popup-scroll-bar-foreground-face ((t (:background ,base05))))
   `(popup-summary-face ((t (:foreground ,base04))))
   `(popup-tip-face ((t (:foreground ,base00 :background ,base0A))))
   `(popup-menu-mouse-face ((t (:foreground ,base00 :background ,base0D))))
   `(popup-menu-selection-face ((t (:foreground ,base00 :background ,base0C))))

;;;; powerline
   `(powerline-active1 ((t (:foreground ,base09 :background ,base00))))
   `(powerline-active2 ((t (:foreground ,base08 :background ,base01))))
   `(powerline-inactive1 ((t (:foreground ,base06 :background ,base01))))
   `(powerline-inactive2 ((t (:foreground ,base07 :background ,base02))))

;;;; python-mode
   `(py-builtins-face ((t (:foreground ,base09 :weight normal))))

;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,base0E))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,base0D))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,base0C))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,base0B))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,base0A))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,base09))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,base08))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,base03))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,base05))))

;;;; regex-tool
   `(regex-tool-matched-face ((t (:foreground nil :background nil :inherit match))))

;;;; rhtml-mode
   `(erb-delim-face ((t (:background ,base03))))
   `(erb-exec-face ((t (:background ,base03 :weight bold))))
   `(erb-exec-delim-face ((t (:background ,base03))))
   `(erb-out-face ((t (:background ,base03 :weight bold))))
   `(erb-out-delim-face ((t (:background ,base03))))
   `(erb-comment-face ((t (:background ,base03 :weight bold :slant italic))))
   `(erb-comment-delim-face ((t (:background ,base03))))

;;;; sh-mode
   `(sh-heredoc ((t (:foreground nil :weight normal :inherit font-lock-string-face))))
   `(sh-quoted-exec ((t (:foreground nil :inherit font-lock-preprocessor-face))))

;;;; show-paren-mode
   `(show-paren-match ((t (:foreground ,base01 :background ,base0D))))
   `(show-paren-mismatch ((t (:foreground ,base01 :background ,base09))))

;;;; slime-mode
   `(slime-highlight-edits-face ((t (:weight bold))))
   `(slime-repl-input-face ((t (:weight normal :underline nil))))
   `(slime-repl-prompt-face ((t (:foreground ,base0E :underline nil :weight bold))))
   `(slime-repl-result-face ((t (:foreground ,base0B))))
   `(slime-repl-output-face ((t (:foreground ,base0D :background ,base01))))

;;;; smart-mode-line
   `(sml/charging ((t (:inherit sml/global :foreground ,base0B))))
   `(sml/discharging ((t (:inherit sml/global :foreground ,base08))))
   `(sml/filename ((t (:inherit sml/global :foreground ,base0A :weight bold))))
   `(sml/global ((t (:foreground ,base04))))
   `(sml/modes ((t (:inherit sml/global :foreground ,base07))))
   `(sml/modified ((t (:inherit sml/not-modified :foreground ,base08 :weight bold))))
   `(sml/outside-modified ((t (:inherit sml/not-modified :background ,base08))))
   `(sml/prefix ((t (:inherit sml/global :foreground ,base09))))
   `(sml/read-only ((t (:inherit sml/not-modified :foreground ,base0C))))

;;;; smerge-mode
   `(smerge-refined-added ((t (:foreground ,base0B))))
   `(smerge-refined-removed ((t (:foreground ,base08))))

;;;; spaceline
   `(spaceline-evil-emacs ((t (:foreground ,base01 :background ,base0D))))
   `(spaceline-evil-insert ((t (:foreground ,base01 :background ,base0D))))
   `(spaceline-evil-motion ((t (:foreground ,base01 :background ,base0E))))
   `(spaceline-evil-normal ((t (:foreground ,base01 :background ,base0B))))
   `(spaceline-evil-replace ((t (:foreground ,base01 :background ,base08))))
   `(spaceline-evil-visual ((t (:foreground ,base01 :background ,base09))))

;;;; spacemacs
   `(spacemacs-emacs-face ((t (:foreground ,base01 :background ,base0D))))
   `(spacemacs-hybrid-face ((t (:foreground ,base01 :background ,base0D))))
   `(spacemacs-insert-face ((t (:foreground ,base01 :background ,base0C))))
   `(spacemacs-motion-face ((t (:foreground ,base01 :background ,base0E))))
   `(spacemacs-lisp-face ((t (:foreground ,base01 :background ,base0E))))
   `(spacemacs-normal-face ((t (:foreground ,base01 :background ,base0B))))
   `(spacemacs-replace-face ((t (:foreground ,base01 :background ,base08))))
   `(spacemacs-visual-face ((t (:foreground ,base01 :background ,base09))))

;;;; structured-haskell-mode
   `(shm-current-face ((t (:inherit region))))
   `(shm-quarantine-face ((t (:underline (:style wave :color ,base08)))))

   ;; telephone-line
   `(telephone-line-accent-active ((t (:foreground ,base00 :background ,base05))))
   `(telephone-line-accent-inactive ((t (:foreground ,base01 :background ,base03))))
   `(telephone-line-evil-normal ((t (:foreground ,base01 :background ,base0B :weight bold))))
   `(telephone-line-evil-insert ((t (:foreground ,base01 :background ,base0D :weight bold))))
   `(telephone-line-evil-visual ((t (:foreground ,base06 :background ,base0E :weight bold))))
   `(telephone-line-evil-replace ((t (:foreground ,base01 :background ,base08 :weight bold))))
   `(telephone-line-evil-operator ((t (:foreground ,base0B :background ,base01 :weight bold))))
   `(telephone-line-evil-motion ((t (:foreground ,base00 :background ,base0C :weight bold))))
   `(telephone-line-evil-emacs ((t (:foreground ,base07 :background ,base0E :weight bold))))
   `(telephone-line-warning ((t (:foreground ,base09 :weight bold))))
   `(telephone-line-error ((t (:foreground ,base08 :weight bold))))

;;;; term and ansi-term
   `(term ((t (:foreground ,base05 :background ,base00))))
   `(term-color-black ((t (:foreground ,base02 :background ,base00))))
   `(term-color-white ((t (:foreground ,base05 :background ,base07))))
   `(term-color-red ((t (:foreground ,base08 :background ,base08))))
   `(term-color-yellow ((t (:foreground ,base0A :background ,base0A))))
   `(term-color-green ((t (:foreground ,base0B :background ,base0B))))
   `(term-color-cyan ((t (:foreground ,base0C :background ,base0C))))
   `(term-color-blue ((t (:foreground ,base0D :background ,base0D))))
   `(term-color-magenta ((t (:foreground ,base0E :background ,base0E))))

;;;; tooltip
   `(tooltip ((t (:background ,base01 :inherit default))))

;;;; tuareg-mode
   `(tuareg-font-lock-governing-face ((t (:weight bold :inherit font-lock-keyword-face))))

;;;; undo-tree-mode
   `(undo-tree-visualizer-default-face ((t (:foreground ,base06))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,base0B :weight bold))))
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,base08))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,base0A))))

;;;; utop-mode
   `(utop-prompt ((t (:foreground ,base0E))))
   `(utop-error ((t (:underline (:style wave :color ,base08) :inherit error))))

;;;; w3m-mode
   `(w3m-anchor ((t (:underline nil :inherit link))))
   `(w3m-anchor-visited ((t (:underline nil :inherit link-visited))))
   `(w3m-form ((t (:foreground ,base09 :underline t))))
   `(w3m-image ((t (:foreground ,base05 :background ,base03))))
   `(w3m-image-anchor ((t (:foreground ,base05 :background ,base03 :underline t))))
   `(w3m-header-line-location-content ((t (:foreground ,base0D :background ,base00))))
   `(w3m-header-line-location-title ((t (:foreground ,base0D :background ,base00))))
   `(w3m-tab-background ((t (:foreground ,base05 :background ,base01))))
   `(w3m-tab-selected ((t (:foreground ,base05 :background ,base00))))
   `(w3m-tab-selected-retrieving ((t (:foreground ,base05 :background ,base00))))
   `(w3m-tab-unselected ((t (:foreground ,base03 :background ,base01))))
   `(w3m-tab-unselected-unseen ((t (:foreground ,base03 :background ,base01))))
   `(w3m-tab-unselected-retrieving ((t (:foreground ,base03 :background ,base01))))

;;;; which-func-mode
   `(which-func ((t (:foreground ,base0D :background nil :weight bold))))

;;;; whitespace-mode
   `(whitespace-empty ((t (:foreground ,base08 :background ,base0A))))
   `(whitespace-hspace ((t (:foreground ,base04 :background ,base04))))
   `(whitespace-indentation ((t (:foreground ,base08 :background ,base0A))))
   `(whitespace-line ((t (:foreground ,base0F :background ,base01))))
   `(whitespace-newline ((t (:foreground ,base04))))
   `(whitespace-space ((t (:foreground ,base03 :background ,base01))))
   `(whitespace-space-after-tab ((t (:foreground ,base08 :background ,base0A))))
   `(whitespace-space-before-tab ((t (:foreground ,base08 :background ,base09))))
   `(whitespace-tab ((t (:foreground ,base03 :background ,base01))))
   `(whitespace-trailing ((t (:foreground ,base0A :background ,base08))))))

(provide-theme 'ef-theme)

(provide 'ef-theme)
