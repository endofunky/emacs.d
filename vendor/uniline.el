;; uniline.el -*- lexical-binding:t -*-

;;
;; Custom
;;

(defgroup uniline nil
  "A new, minimal mode-line."
  :group 'mode-line)

(defgroup uniline-faces nil
  "The faces of `uniline'."
  :group 'uniline
  :group 'faces)

(defface uniline
  `((t (:inherit mode-line
        :box (:line-width (1 . 7)
              :color ,(face-background 'mode-line))
        )))
  "Face used for default."
  :group 'uniline-faces)

(defface uniline-inactive
  `((t (:inherit mode-line-inactive
        :box (:line-width (1 . 7)
              :color ,(face-background 'mode-line-inactive)))))
  "Face used for inactive."
  :group 'uniline-faces)

(defface uniline-highlight
  `((t (:inherit mode-line-highlight
        :box (:line-width (1 . 7)
              :color ,(face-background 'mode-line-inactive)))))
  "Face used for inactive."
  :group 'uniline-faces)

(defface uniline-spc-face
  '((t (:inherit uniline)))
  "Face used for the white space."
  :group 'uniline-faces)

(defface uniline-major-mode-face
  '((t (:inherit (font-lock-builtin-face uniline))))
  "Face used for the major-mode segment in the mode-line."
  :group 'uniline-faces)

(defface uniline-buffer-name-face
  '((t (:inherit (font-lock-builtin-face uniline))))
  "Face used for the buffer name segment in the mode-line."
  :group 'uniline-faces)

(defface uniline-project-face
  '((t (:inherit (uniline))))
  "Face used for the project segment in the mode-line."
  :group 'uniline-faces)

(defface uniline-lsp-face
  '((t (:inherit (bold uniline))))
  "Face used for the buffer LSP segment in the mode-line."
  :group 'uniline-faces)

(defface uniline-position-face
  '((t (:inherit uniline)))
  "Face used for the position segment in the mode-line."
  :group 'uniline-faces)

(defface uniline-ok-face
  '((t :inherit (success uniline)))
  "Face for ok status in the mode-line.")

(defface uniline-vcs-face
  '((t :inherit (font-lock-keyword-face uniline)))
  "Face for warning status in the mode-line.")

(defface uniline-warning-face
  '((t :inherit (warning uniline)))
  "Face for warning status in the mode-line.")

(defface uniline-error-face
  '((t :inherit (error uniline)))
  "Face for error status in the mode-line.")

(defface uniline-ro-face
  '((t :inherit (error uniline)))
  "Face for error status in the mode-line.")

(defface uniline-panel
  '((t :inherit font-lock-builtin-face :inverse-video t))
  "Face for error status in the mode-line.")

(defface uniline-record
  '((t :inherit error :inverse-video t))
  "Face for error status in the mode-line.")

(defface uniline-panel-warning
  '((t :inherit warning :inverse-video t))
  "Face for error status in the mode-line.")

(defface uniline-emacs-state-face
  '((t :inherit font-lock-keyword-face :inverse-video t))
  "Face for error status in the mode-line.")

(require 'thingatpt)

;;
;; Temp vars
;;

(defvar uniline--original-mode-line-format)
(defvar uniline--mode-line-format)

;;
;; Defintions for byte-compiler
;;
(declare-function all-the-icons--function-name "ext:all-the-icons")

(defvar anzu-cons-mode-line-p)
(defvar anzu--current-position)
(defvar anzu--total-matched)
(defvar anzu--cached-count)
(defvar anzu--state)
(defvar anzu--overflow-p)
(declare-function anzu--reset-status "ext:anzu")
(declare-function anzu--where-is-here "ext:anzu")

(declare-function eglot "ext:eglot")
(declare-function eglot--major-mode "ext:eglot")
(declare-function eglot--project-nickname "ext:eglot")
(declare-function eglot--server-info "ext:eglot")
(declare-function eglot--spinner "ext:eglot")
(declare-function eglot-clear-status "ext:eglot")
(declare-function eglot-current-server "ext:eglot")
(declare-function eglot-events-buffer "ext:eglot")
(declare-function eglot-forget-pending-continuations "ext:eglot")
(declare-function eglot-managed-p "ext:eglot")
(declare-function eglot-reconnect "ext:eglot")
(declare-function eglot-shutdown "ext:eglot")
(declare-function eglot-stderr-buffer "ext:eglot")

(declare-function jsonrpc-last-error "ext:jsonrpc")
(declare-function jsonrpc--request-continuations "ext:jsonrpc")

(defvar evil-state)
(defvar evil-mode)
(defvar evil-mode-line-tag)
(declare-function evil-state-property "ext:evil-common")
(declare-function evil-force-normal-state "ext:evil-states")
(declare-function evil-emacs-state-p "ext:evil-states")
(declare-function evil-insert-state-p "ext:evil-states")
(declare-function evil-motion-state-p "ext:evil-states")
(declare-function evil-visual-state-p "ext:evil-states")
(declare-function evil-replace-state-p "ext:evil-states")

(defvar flycheck-mode)
(defvar flycheck-last-status-change)
(defvar flycheck-current-errors)
(declare-function flycheck-count-errors "ext:flycheck")
(declare-function flycheck-list-errors "ext:flycheck")
(declare-function flycheck-error-list-current-errors "ext:flycheck")

(declare-function flyspell-overlay-p "ext:flyspell")
(declare-function flyspell-get-word "ext:flyspell")

(declare-function magit-get-push-branch "ext:magit-git")
(declare-function magit-get-current-branch "ext:magit-git")
(declare-function magit-git-string "ext:magit-git")

(declare-function project-root "project")

;;
;; Helpers
;;

(cl-defun uniline--icon (set name fallback
                             &key (face 'uniline) (v-adjust 0))
  (if window-system
      (when-let* ((func (all-the-icons--function-name set))
                  (icon (and (fboundp func)
                             (apply func (list name :face face
                                               :v-adjust v-adjust)))))
        (when-let ((props (get-text-property 0 'face icon)))
          (when (listp props)
            (cl-destructuring-bind (&key family _height inherit
                                         &allow-other-keys)
                props
              (propertize icon
                          'face `(:inherit ,(or face inherit props 'uniline)
                                  :weight normal
                                  :family  ,(or family "")))))))
    (propertize fallback 'face face)))

(defun uniline--face (face &optional inactive-face)
  "Display FACE in mode-line.
If INACTIVE-FACE is nil, will use `mode-line-inactive' face."
  (if (uniline--active)
      face
    (or inactive-face 'uniline-inactive)))

(defun uniline--format (left-segments right-segments)
  "Return a string of `window-width' length containing LEFT-SEGMENTS and
RIGHT-SEGMENTS, aligned respectively."
  (let* ((lhs (uniline--format-segments left-segments))
         (rhs (uniline--format-segments right-segments)))
    (concat
     lhs
     (propertize " "
                 'display `((space
                             :align-to
                             (- (+ right right-fringe right-margin scroll-bar)
                                ,(string-width
                                  rhs))))
                 'face '(:inherit uniline))
     rhs)))

(defun uniline--format-segments (segments)
  "Return a string from a list of SEGMENTS."
  (format-mode-line
   (mapcar
    (lambda (segment)
      `(:eval (let* ((s (concat (,segment)))
                     (end (length s)))
                (if (uniline--active)
                    s
                  (add-face-text-property 0 end 'uniline-inactive nil s)
                  s))))
    segments)))

(defun uniline--force-refresh (format)
  "Updates the modeline format in each buffer."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (setq mode-line-format format))))

(defsubst uniline-spc ()
  "Text style with whitespace."
  (propertize " " 'face 'uniline-spc-face))

;;
;; Current window tracking (from uniline)
;;

(defun uniline--get-current-window (&optional frame)
  "Get the current window but should exclude the child windows.
If FRAME is nil, it means the current frame."
  (if (and (fboundp 'frame-parent) (frame-parent frame))
      (frame-selected-window (frame-parent frame))
    (frame-selected-window frame)))

(defvar uniline-current-window (uniline--get-current-window))

(defun uniline--active ()
  "Whether is an active window."
  (unless (and (bound-and-true-p mini-frame-frame)
               (and (frame-live-p mini-frame-frame)
                    (frame-visible-p mini-frame-frame)))
    (and uniline-current-window
         (eq (uniline--get-current-window) uniline-current-window))))

(defun uniline-set-selected-window (&rest _)
  "Set `uniline-current-window' appropriately."
  (let ((win (uniline--get-current-window)))
    (setq uniline-current-window
          (if (minibuffer-window-active-p win)
              (minibuffer-selected-window)
            win))))

(defun uniline-unset-selected-window ()
  "Unset `uniline-current-window' appropriately."
  (setq uniline-current-window nil))

(add-hook 'pre-redisplay-functions #'uniline-set-selected-window)

;;
;; Segments
;;

(defun uniline-major-mode (&rest _)
  "The major mode, including environment and text-scale info."
  (propertize
   (concat
    (propertize (format-mode-line
                 (or (and (boundp 'delighted-modes)
                          (cadr (assq major-mode delighted-modes)))
                     mode-name))
                'help-echo "Major mode\n\
  mouse-1: Display major mode menu\n\
  mouse-2: Show help for major mode\n\
  mouse-3: Toggle minor modes"
                'mouse-face 'uniline-highlight
                'local-map mode-line-major-mode-keymap)
    (and (boundp 'text-scale-mode-amount)
         (/= text-scale-mode-amount 0)
         (format
          (if (> text-scale-mode-amount 0)
              " (%+d)"
            " (%-d)")
          text-scale-mode-amount))
    (uniline-spc))
   'face 'uniline-major-mode-face))

(defvar-local uniline--vcs-text nil)
(defvar-local uniline--vcs-icon nil)

(defun uniline-vcs-text (&rest _)
  uniline--vcs-text)

(defun uniline-vcs-icon (&rest _)
  uniline--vcs-icon)

(defun uniline--update-vcs (&rest _)
  (when (and vc-mode buffer-file-name)
    (let* ((backend (vc-backend buffer-file-name))
           (state (vc-state (file-local-name buffer-file-name) backend))
           (str (if vc-display-status
                    (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                  "")))
      (setq uniline--vcs-icon
            (concat (cond ((memq state '(edited added))
                           (uniline--icon 'octicon "git-compare" "⇆"
                                          :face 'uniline-warning-face))
                          ((eq state 'needs-merge)
                           (uniline--icon 'octicon "git-merge" "⛙"
                                          :face 'uniline-warning-face))
                          ((eq state 'needs-update)
                           (uniline--icon 'octicon "arrow-down" "↓"
                                          :face 'uniline-warning-face))
                          ((memq state '(removed conflict unregistered))
                           (uniline--icon 'octicon "alert" "⚠"
                                          :face 'uniline-error-face))
                          (t
                           (uniline--icon 'octicon "git-branch" "@"
                                          :face 'uniline-vcs-face)))
                    (uniline-spc)))
      (setq uniline--vcs-text
            (concat
             (propertize (if (length> str 25)
                             (concat
                              (substring str 0 (- 25 3))
                              "...")
                           str)
                         'face (cond
                                ((eq state '(needs-update needs-merge))
                                 'uniline-warning-face)
                                ((memq state '(removed conflict unregistered))
                                 'uniline-error-face)
                                ((memq state '(edited added))
                                 'uniline-warning-face)
                                (t 'uniline-vcs-face)))
             (uniline-spc))))))

(defvar-local uniline--git-unpushed-icon nil)
(defvar-local uniline--git-unpulled-icon nil)
(defvar-local uniline--git-unpushed-text nil)
(defvar-local uniline--git-unpulled-text nil)

(defun uniline-git-unpulled-icon (&rest _)
  uniline--git-unpulled-icon)

(defun uniline-git-unpushed-icon (&rest _)
  uniline--git-unpushed-icon)

(defun uniline-git-unpulled-text (&rest _)
  uniline--git-unpulled-text)

(defun uniline-git-unpushed-text (&rest _)
  uniline--git-unpushed-text)

(defconst uniline--magit-counts-max 256)

(defun uniline--magit-counts ()
  "If the current branch has a corresponding upstream branch, returns a cons
pair with the unpulled and unpushed commits, nil otherwise.

(unpulled unpushed)

Will return a maximum count of 256 for each."
  (if-let* ((remote (magit-get-push-branch))
            (local (magit-get-current-branch))
            (result (magit-git-string "rev-list" "--left-right" "--count"
                                      (format "-n%d" uniline--magit-counts-max)
                                      (concat remote "..." local))))
      (mapcar #'string-to-number
              (split-string result "\t" t))))

(defun uniline--update-git (&rest _)
  (setq uniline--git-unpushed-icon nil)
  (setq uniline--git-unpulled-icon nil)
  (setq uniline--git-unpushed-text nil)
  (setq uniline--git-unpulled-text nil)
  (when (and buffer-file-name
             (fboundp 'magit-git-string)
             (string= "Git" (vc-backend buffer-file-name)))
    (when-let* ((counts (uniline--magit-counts)))
      (let ((unpulled (car counts))
            (unpushed (cadr counts)))
        (when (> unpulled 0)
          (setq uniline--git-unpulled-icon
                (uniline--icon 'octicon "arrow-down" "↓"
                               :face 'uniline-warning-face
                               :v-adjust -0.04))
          (setq uniline--git-unpulled-text
                (concat (propertize (number-to-string unpulled)
                                    'face 'uniline-warning-face)
                        (uniline-spc))))
        (when (> unpushed 0)
          (setq uniline--git-unpushed-icon
                (uniline--icon 'octicon "arrow-up" "↑"
                               :face 'uniline-warning-face
                               :v-adjust 0.05))
          (setq uniline--git-unpushed-text
                (concat (propertize (number-to-string unpushed)
                                    'face 'uniline-warning-face)
                        (uniline-spc))))))))

(defvar-local uniline--bisect-text nil)

(defun uniline-git-bisect (&rest _)
  uniline--bisect-text)

(defun uniline--update-git-bisect (&rest _)
  (setq uniline--bisect-text
        (when (and (fboundp 'magit-bisect-in-progress-p)
                   (string= "Git" (vc-backend buffer-file-name))
                   (magit-bisect-in-progress-p))
          (concat
           (propertize "Bisect"
                       'face (uniline--face 'uniline-error-face)
                       'help-echo "Git-bisect in progress\n\
mouse-1: Open bisect menu\n\
mouse-2: magit-bisect-bad\n\
mouse-3: magit-bisect-good"
                       'mouse-face 'uniline-highlight
                       'local-map (let ((map (make-sparse-keymap)))
                                    (define-key map
                                      [mode-line mouse-1] 'magit-bisect)
                                    [mode-line mouse-2] 'magit-bisect-bad
                                    [mode-line mouse-3] 'magit-bisect-good
                                    map))
           (uniline-spc)))))

(defun uniline-eol (&rest _)
  "Displays the eol style of the buffer."
  (let ((face 'uniline)
        (mouse-face 'uniline-highlight))
    (let ((eol (coding-system-eol-type buffer-file-coding-system)))
      (propertize
       (pcase eol
         (0 "LF")
         (1 "CRLF")
         (2 "CR")
         (_ ""))
       'face face
       'mouse-face mouse-face
       'help-echo (format "End-of-line style: %s\nmouse-1: Cycle"
                          (pcase eol
                            (0 "Unix-style LF")
                            (1 "DOS-style CRLF")
                            (2 "Mac-style CR")
                            (_ "Undecided")))
       'local-map (let ((map (make-sparse-keymap)))
                    (define-key map [mode-line mouse-1] 'mode-line-change-eol)
                    map)))))

(defun uniline-encoding (&rest _)
  "Displays the encoding of the buffer."
  (let ((face 'uniline)
        (mouse-face 'uniline-highlight))
    (let* ((sys (coding-system-plist buffer-file-coding-system))
           (cat (plist-get sys :category))
           (sym (if (memq cat
                          '(coding-category-undecided coding-category-utf-8))
                    'utf-8
                  (plist-get sys :name))))
      (propertize
       (upcase (symbol-name sym))
       'face face
       'mouse-face mouse-face
       'help-echo 'mode-line-mule-info-help-echo
       'local-map mode-line-coding-system-map))))

(defun uniline-buffer-name (&rest _)
  "Update buffer file name in mode-line."
  (if (and buffer-file-name
           (buffer-modified-p (current-buffer)))
      (propertize "%b"
                  'face 'uniline-error-face
                  'mouse-face 'uniline-highlight
                  'help-echo "Buffer name
mouse-1: Previous buffer\nmouse-3: Next buffer"
                  'local-map mode-line-buffer-identification-keymap)
    (propertize "%b"
                'face 'uniline-buffer-name-face
                'mouse-face 'uniline-highlight
                'help-echo "Buffer name
mouse-1: Previous buffer\nmouse-3: Next buffer"
                'local-map mode-line-buffer-identification-keymap)))

(defun uniline-position (&rest _)
  (propertize ":%l:%c "
              'face 'uniline-position-face))

(defvar-local uniline--flycheck-text nil)
(defvar-local uniline--flycheck-icon nil)
(defvar-local uniline--flycheck-error-text nil)
(defvar-local uniline--flycheck-error-icon nil)
(defvar-local uniline--flycheck-warning-text nil)
(defvar-local uniline--flycheck-warning-icon nil)
(defvar-local uniline--flycheck-info-text nil)
(defvar-local uniline--flycheck-info-icon nil)

(defun uniline-flycheck-text (&rest _)
  uniline--flycheck-text)

(defun uniline--flycheck-update (&rest _)
  "Return the status of flycheck to be displayed in the mode-line."
  (setq uniline--flycheck-text nil)
  (when (and (fboundp 'flycheck-mode)
             flycheck-mode)
    (let ((help-echo "Show Flycheck Errors")
          (local-map (make-mode-line-mouse-map
                      'mouse-1 #'flycheck-list-errors)))
      (pcase flycheck-last-status-change
        (`finished
         (if flycheck-current-errors
             (let* ((errors
                     (flycheck-count-errors flycheck-current-errors))
                    (info-count (or (alist-get 'info errors) 0))
                    (warning-count (or (alist-get 'warning errors) 0))
                    (error-count (or (alist-get 'error errors) 0))
                    (count (+ info-count warning-count error-count)))
               (setq
                uniline--flycheck-text
                (concat (propertize
                         (format "%s Issue%s" count (if (eq 1 count) "" "s"))
                         'face (cond ((> error-count 0) 'uniline-error-face)
                                     ((> warning-count 0) 'uniline-warning-face)
                                     (t 'uniline-ok-face))
                         'help-echo help-echo
                         'local-map local-map)
                        (uniline-spc))))
           (setq uniline--flycheck-text
                 (concat
                  (propertize "No Issues"
                              'face 'uniline-ok-face
                              'help-echo help-echo
                              'local-map local-map)
                  (uniline-spc)))))
        (`errored
         (setq uniline--flycheck-text
               (concat
                (propertize "Error"
                            'face 'uniline-error-face
                            'help-echo help-echo
                            'local-map local-map)
                (uniline-spc))))
        (`interrupted
         (setq uniline--flycheck-text
               (concat
                (propertize "Interrupted"
                            'face 'uniline-error-face
                            'help-echo help-echo
                            'local-map local-map)
                (uniline-spc))))
        (`suspicious t)))))

(defun uniline-evil--tag ()
  (let ((help-echo (evil-state-property evil-state :name))
        (mouse-face 'uniline-highlight))
    (concat
     (cond
      ((evil-emacs-state-p)
       (propertize " Emacs "
                   'face (uniline--face 'uniline-emacs-state-face)
                   'help-echo help-echo
                   'mouse-face mouse-face))
      ((evil-insert-state-p)
       (propertize " Insert "
                   'face (uniline--face 'uniline-panel-warning)
                   'help-echo help-echo
                   'mouse-face mouse-face))
      ((evil-motion-state-p)
       (propertize " Motion "
                   'face (uniline--face 'uniline-panel)
                   'help-echo help-echo
                   'mouse-face mouse-face))
      ((evil-visual-state-p)
       (propertize " Visual "
                   'face (uniline--face 'uniline-panel)
                   'help-echo help-echo
                   'mouse-face mouse-face))
      ((evil-replace-state-p)
       (propertize " Replace "
                   'face (uniline--face 'uniline-record)
                   'help-echo help-echo
                   'mouse-face mouse-face)))
     (uniline-spc))))

(defun uniline-evil (&rest _)
  (when (and (fboundp 'evil-mode)
             evil-mode)
    (uniline-evil--tag)))

(defun uniline-evil-unless-emacs (&rest _)
  (when (and (fboundp 'evil-mode)
             evil-mode)
    (unless (or (bound-and-true-p anzu--state))
      (if (eq evil-state 'emacs)
          " "
        (uniline-evil--tag)))))

(defun uniline-ro (&rest _)
  (when buffer-read-only
    (concat
     (uniline-spc)
     (propertize "RO" 'face 'uniline-ro-face))))

(defun uniline-project (&rest _)
  (when-let* ((project (project-current nil))
              (root (project-root project))
              (parts (file-name-split root)))
    (concat
     (propertize (nth (- (length parts) 2) parts) 'face 'uniline-project-face)
     (uniline-spc))))

;; `anzu' and `evil-anzu' expose current/total state that can be displayed in
;; the mode-line.
(defun uniline-fix-anzu-count (positions here)
  "Calulate anzu count via POSITIONS and HERE."
  (cl-loop for (start . end) in positions
           collect t into before
           when (and (>= here start) (<= here end))
           return (length before)
           finally return 0))

(advice-add #'anzu--where-is-here :override #'uniline-fix-anzu-count)

(setq anzu-cons-mode-line-p nil) ; manage modeline segment ourselves
;; Ensure anzu state is cleared when searches & iedit are done
(with-eval-after-load 'anzu
  (add-hook 'isearch-mode-end-hook #'anzu--reset-status t)
  (add-hook 'iedit-mode-end-hook #'anzu--reset-status)
  (advice-add #'evil-force-normal-state :after #'anzu--reset-status)
  ;; Fix matches segment mirroring across all buffers
  (mapc #'make-variable-buffer-local
        '(anzu--total-matched
          anzu--current-position anzu--state anzu--cached-count
          anzu--cached-positions anzu--last-command
          anzu--last-isearch-string anzu--overflow-p)))

(defun uniline--anzu (&rest _)
  "Show the match index and total number thereof.
Requires `anzu', also `evil-anzu' if using `evil-mode' for compatibility with
`evil-search'."
  (when (and (uniline--active)
             (bound-and-true-p anzu--state)
             (not (bound-and-true-p iedit-mode)))
    (concat
     (propertize
      (let ((here anzu--current-position)
            (total anzu--total-matched))
        (cond ((eq anzu--state 'replace-query)
               (format " %d replace " anzu--cached-count))
              ((eq anzu--state 'replace)
               (format " %d/%d " here total))
              (anzu--overflow-p
               (format " %s+ " total))
              (t
               (format " %s/%d " here total))))
      'face 'uniline-panel)
     (uniline-spc))))

(defun uniline-macrostep (&rest _)
  (when (bound-and-true-p macrostep-mode)
    (concat
     (propertize
      " Macrostep "
      'face (uniline--face 'uniline-panel-warning)))))

(defsubst uniline-macro (&rest _)
  "Display current Emacs or evil macro being recorded."
  (when (and (uniline--active)
             (or defining-kbd-macro executing-kbd-macro))
    (let ((sep (propertize " " 'face 'uniline-record )))
      (concat
       sep
       (propertize
        (concat "● " (if (bound-and-true-p evil-this-macro)
                         (char-to-string evil-this-macro)
                       "Macro"))
        'face 'uniline-record)
       sep))))

(defun uniline-misc (&rest _)
  (format-mode-line mode-line-misc-info))

(defun uniline-lsp ()
  "Update `eglot' state."
  (when (and (fboundp 'eglot-managed-p)
             (eglot-managed-p))
    (pcase-let* ((server (and (eglot-managed-p)
                              (eglot-current-server)))
                 (nick (and server (eglot--project-nickname server)))
                 (pending (and server (hash-table-count
                                       (jsonrpc--request-continuations server))))
                 (`(,_id ,doing ,done-p ,detail) (and server
                                                      (eglot--spinner server)))
                 (last-error (and server (jsonrpc-last-error server)))
                 (face (cond (last-error 'uniline-error-face)
                             ((and doing (not done-p)) 'uniline-ok-face)
                             ((and pending (cl-plusp pending))
                              'uniline-warning-face)
                             (nick 'uniline-lsp-face)
                             (t 'uniline-warning-face))))
      (concat
       (propertize (plist-get (eglot--server-info server) :name)
                   'face (uniline--face face)
                   'help-echo (cond
                               (last-error
                                (format "EGLOT\nAn error occured: %s
mouse-3: Clear this status" (plist-get last-error :message)))
                               ((and doing (not done-p))
                                (format "EGLOT\n%s%s" doing
                                        (if detail (format "%s" detail) "")))
                               ((and pending (cl-plusp pending))
                                (format "EGLOT\n%d outstanding requests" pending))
                               (nick (format "EGLOT Connected (%s/%s)
C-mouse-1: Go to server errors
mouse-1: Go to server events
mouse-2: Quit server
mouse-3: Reconnect to server" nick (eglot--major-mode server)))
                               (t "EGLOT Disconnected
mouse-1: Start server"))
                   'mouse-face 'uniline-highlight
                   'local-map (let ((map (make-sparse-keymap)))
                                (cond (last-error
                                       (define-key map [mode-line mouse-3]
                                         #'eglot-clear-status))
                                      ((and pending (cl-plusp pending))
                                       (define-key map [mode-line mouse-3]
                                         #'eglot-forget-pending-continuations))
                                      (nick
                                       (define-key map [mode-line C-mouse-1]
                                         #'eglot-stderr-buffer)
                                       (define-key map [mode-line mouse-1]
                                         #'eglot-events-buffer)
                                       (define-key map [mode-line mouse-2]
                                         #'eglot-shutdown)
                                       (define-key map [mode-line mouse-3]
                                         #'eglot-reconnect))
                                      (t (define-key map [mode-line mouse-1]
                                           #'eglot)))
                                map))
       (uniline-spc)))))

(defun uniline-override-eglot-modeline ()
  "Override `eglot' mode-line."
  (if (bound-and-true-p uniline-mode)
      (setq mode-line-misc-info
            (delq (assq 'eglot--managed-mode mode-line-misc-info) mode-line-misc-info))
    (add-to-list 'mode-line-misc-info
                 `(eglot--managed-mode (" [" eglot--mode-line-format "] ")))))
(add-hook 'eglot-managed-mode-hook #'uniline-override-eglot-modeline)
(add-hook 'uniline-mode-hook #'uniline-override-eglot-modeline)

(defun uniline--has-flyspell-overlay-p (ovs)
  (let ((r nil))
    (while (and (not r) (consp ovs))
      (if (flyspell-overlay-p (car ovs))
          (setq r t)
        (setq ovs (cdr ovs))))
    r))

(defun uniline--collect-flyspell-candidates ()
  (save-excursion
    (save-restriction
      (narrow-to-region (window-start) (window-end (selected-window) t))
      (let ((pos (point-min))
            (pos-max (point-max))
            (pos-list nil)
            (word t))
        (goto-char pos)
        (while (and word (< pos pos-max))
          (setq word (flyspell-get-word t))
          (when word
            (setq pos (nth 1 word))
            (let* ((ovs (overlays-at pos))
                   (r (uniline--has-flyspell-overlay-p ovs)))
              (when r
                (push pos pos-list)))
            (setq pos (1+ (nth 2 word)))
            (goto-char pos)))
        (nreverse pos-list)))))

(defun uniline-flyspell (&rest _)
  (when (and (boundp 'flyspell-mode)
             flyspell-mode
             (or (eq major-mode 'markdown-mode)
                 (eq major-mode 'gfm-mode)
                 (eq major-mode 'org-mode)
                 (eq major-mode 'text-mode)))
    (let ((count (length (uniline--collect-flyspell-candidates))))
      (when (> count 0)
        (propertize (if (> count 1)
                        (format "%d Misspells " count)
                      "1 Misspell ")
                    'help-echo "Flyspell: mouse-1: Correct next word"
                    'local-map (let ((map (make-sparse-keymap)))
                                 (define-key map [mode-line mouse-1]
                                   'flyspell-correct-next)
                                 map)
                    'face 'uniline-error-face)))))
;;
;; Mode-specific mode-line formats
;;
(defun uniline--flycheck-error-details (&rest _)
  (let* ((counts (flycheck-count-errors (flycheck-error-list-current-errors)))
         (error-count (or (alist-get 'error counts) 0))
         (warning-count (or (alist-get 'warning counts) 0))
         (info-count (or (alist-get 'info counts) 0)))
    (concat
     (if (and (= error-count 0)
              (= warning-count 0)
              (= info-count 0))
         (propertize "No Errors" 'face 'uniline-ok-face))
     (if (> error-count 0)
         (propertize (if (> error-count 1)
                         (format "%d Errors " error-count)
                       "1 Error")
                     'face 'uniline-error-face))
     (if (> warning-count 0)
         (propertize (if (> warning-count 1)
                         (format "%d Warnings " warning-count)
                       "1 Warning")
                     'face 'uniline-warning-face))
     (if (> info-count 0)
         (propertize (format "%d Infomational " info-count)
                     'face 'uniline-ok-face)))))

(defun uniline--set-mini-format ()
  (setq mode-line-format
        '(:eval
          (uniline--format
           ;; LHS
           '(uniline--anzu
             uniline-evil
             uniline-buffer-name
             uniline-position
             uniline-misc)
           ;; RHS
           '(uniline-major-mode)))))

(defun uniline--set-flycheck-format ()
  (setq mode-line-format
        '(:eval
          (uniline--format
           ;; LHS
           '(uniline--anzu
             uniline-evil
             uniline--flycheck-error-details
             uniline-misc)
           ;; RHS
           '(uniline-major-mode)))))

(defun uniline--set-vterm-format ()
  (setq mode-line-format
        '(:eval
          (uniline--format
           ;; LHS
           '(uniline--anzu
             uniline-evil-unless-emacs
             uniline-buffer-name
             uniline-misc)
           ;; RHS
           '(uniline-major-mode)))))

;;
;; Mode
;;

(define-minor-mode uniline-mode
  "Toggle `uniline' on or off."
  :group 'uniline
  :global t
  :lighter nil
  (if uniline-mode
      (progn
        (setq uniline--original-mode-line-format mode-line-format)

        (setq uniline--mode-line-format
              '(:eval
                (uniline--format
                 ;; LHS
                 '(uniline-macro
                   uniline-macrostep
                   uniline--anzu
                   uniline-evil
                   uniline-project
                   uniline-buffer-name
                   uniline-position
                   uniline-misc)
                 ;; RHS
                 '(uniline-flyspell
                   uniline-flycheck-text
                   uniline-vcs-icon
                   uniline-vcs-text
                   uniline-git-bisect
                   uniline-git-unpulled-icon
                   uniline-git-unpulled-text
                   uniline-git-unpushed-icon
                   uniline-git-unpushed-text
                   uniline-major-mode
                   uniline-lsp
                   uniline-encoding
                   uniline-spc
                   uniline-eol
                   uniline-ro
                   uniline-spc))))

        (setq-default mode-line-format uniline--mode-line-format)

        (add-hook 'flycheck-status-changed-functions #'uniline--flycheck-update)
        (add-hook 'flycheck-mode-hook #'uniline--flycheck-update)
        (add-hook 'flycheck-error-list-mode-hook #'uniline--set-flycheck-format)

        (add-hook 'process-menu-mode-hook #'uniline--set-mini-format)

        (add-hook 'vterm-mode-hook #'uniline--set-vterm-format)

        (add-hook 'find-file-hook #'uniline--update-vcs)
        (add-hook 'after-save-hook #'uniline--update-vcs)
        (advice-add #'vc-refresh-state :after #'uniline--update-vcs)

        (add-hook 'find-file-hook #'uniline--update-git)
        (add-hook 'after-save-hook #'uniline--update-git)
        (advice-add #'vc-refresh-state :after #'uniline--update-git)

        (add-hook 'find-file-hook #'uniline--update-git-bisect)
        (add-hook 'after-save-hook #'uniline--update-git-bisect)
        (advice-add #'vc-refresh-state :after #'uniline--update-git-bisect)

        (uniline--force-refresh uniline--mode-line-format))
    (progn
      ;; Reset the original modeline state
      (setq-default mode-line-format uniline--original-mode-line-format)

      (remove-hook 'flycheck-status-changed-functions
                   #'uniline--flycheck-update)
      (remove-hook 'flycheck-mode-hook #'uniline--flycheck-update)
      (remove-hook 'flycheck-error-list-mode-hook
                   #'uniline--set-flycheck-format)

      (remove-hook 'process-menu-mode-hook #'uniline--set-mini-format)

      (remove-hook 'vterm-mode-hook #'uniline--set-vterm-format)

      (remove-hook 'find-file-hook #'uniline--update-vcs)
      (remove-hook 'after-save-hook #'uniline--update-vcs)
      (advice-remove #'vc-refresh-state #'uniline--update-vcs)

      (remove-hook 'find-file-hook #'uniline--update-git)
      (remove-hook 'after-save-hook #'uniline--update-git)
      (advice-remove #'vc-refresh-state #'uniline--update-git)

      (remove-hook 'find-file-hook #'uniline--update-git-bisect)
      (remove-hook 'after-save-hook #'uniline--update-git-bisect)
      (advice-remove #'vc-refresh-state #'uniline--update-git-bisect)

      (uniline--force-refresh uniline--original-mode-line-format)
      (setq uniline--original-mode-line-format nil))))

(provide 'uniline)
