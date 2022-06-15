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
        :height 0.9
        :box (:line-width (1 . 6)
              :color ,(face-background 'mode-line)))))
  "Face used for default."
  :group 'uniline-faces)

(defface uniline-inactive
  `((t (:inherit mode-line-inactive
        :height 0.9
        :box (:line-width (1 . 6)
              :color ,(face-background 'mode-line-inactive)))))
  "Face used for inactive."
  :group 'uniline-faces)

(defface uniline-highlight
  `((t (:inherit mode-line-highlight
        :height 0.9
        :box (:line-width (1 . 6)
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

(defface uniline-position-face
  '((t (:inherit uniline)))
  "Face used for the position segment in the mode-line."
  :group 'uniline-faces)

(defface uniline-ok-face
  '((t :inherit (success uniline)))
  "Face for ok status in the mode-line.")

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

;;
;; Temp vars
;;

(defvar uniline--original-mode-line-format)
(defvar uniline--mode-line-format)

;;
;; Defintions for byte-compiler
;;
(defvar anzu-cons-mode-line-p)
(defvar anzu--current-position)
(defvar anzu--total-matched)
(defvar anzu--cached-count)
(defvar anzu--state)
(defvar anzu--overflow-p)
(declare-function anzu--reset-status "ext:anzu")
(declare-function anzu--where-is-here "ext:anzu")

(defvar evil-mode)
(defvar evil-mode-line-tag)
(declare-function evil-force-normal-state "ext:evil-states")

(defvar flycheck-mode)
(defvar flycheck-last-status-change)
(defvar flycheck-current-errors)
(declare-function flycheck-count-errors "ext:flycheck")
(declare-function flycheck-list-errors "ext:flycheck")

(defvar lsp--buffer-workspaces)
(declare-function lsp--workspace-print "ext:lsp-mode")
(declare-function lsp-describe-session "ext:lsp-mode")
(declare-function lsp-workspace-folders-open "ext:lsp-mode")
(declare-function lsp-workspace-restart "ext:lsp-mode")
(declare-function lsp-workspace-shutdown "ext:lsp-mode")
(declare-function lsp-workspaces "ext:lsp-mode")

;;
;; Helpers
;;

(defun uniline--face (face &optional inactive-face)
  "Display FACE in mode-line.
If INACTIVE-FACE is nil, will use `mode-line-inactive' face."
  (if (uniline--active)
      face
    (or inactive-face 'uniline-inactive)))

(defun uniline--format (left-segments right-segments)
  "Return a string of `window-width' length containing LEFT-SEGMENTS and
RIGHT-SEGMENTS, aligned respectively."
  (let* ((left (uniline--format-segments left-segments))
         (right (uniline--format-segments right-segments))
         (reserve (length right)))
    (concat
     left
     (propertize " "
                 'display `((space :align-to (- right ,reserve)))
                 'face '(:inherit uniline))
     right)))

(defun uniline--format-segments (segments)
  "Return a string from a list of SEGMENTS."
  (format-mode-line (mapcar
                     (lambda (segment)
                       `(:eval (,segment)))
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

(defun uniline-major-mode ()
  "The major mode, including environment and text-scale info."
  (propertize
   (concat
    (uniline-spc)
    (propertize (format-mode-line
                 (or (and (boundp 'delighted-modes)
                          (cadr (assq major-mode delighted-modes)))
                     mode-name))
                'help-echo "Major mode\n\
  mouse-1: Display major mode menu\n\
  mouse-2: Show help for major mode\n\
  mouse-3: Toggle minor modes"
                'mouse-face 'unilight-highlight
                'local-map mode-line-major-mode-keymap)
    (and (boundp 'text-scale-mode-amount)
         (/= text-scale-mode-amount 0)
         (format
          (if (> text-scale-mode-amount 0)
              " (%+d)"
            " (%-d)")
          text-scale-mode-amount))
    (uniline-spc))
   'face (uniline--face 'uniline-major-mode-face)))

(defun uniline-encoding ()
  "Displays the eol and the encoding style of the buffer."
  (let ((face (uniline--face 'uniline))
        (mouse-face 'uniline-highlight))
    (concat
     ;; coding system
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
        'local-map mode-line-coding-system-map))

     " "

     ;; eol type
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
                     map))))))

(defun uniline-buffer-mark (&rest _)
  "Update buffer file name mark in mode-line."
  (when buffer-file-name
    (propertize (if (buffer-modified-p (current-buffer)) "✖ " "✔ ")
                'face (if (buffer-modified-p (current-buffer))
                          (uniline--face 'uniline-warning-face)
                        (uniline--face 'uniline-ok-face)))))

(defun uniline-buffer-name (&rest _)
  "Update buffer file name in mode-line."
  (propertize "%b"
              'face (uniline--face 'uniline-buffer-name-face)
              'mouse-face 'uniline-highlight
              'help-echo "Buffer name
mouse-1: Previous buffer\nmouse-3: Next buffer"
              'local-map mode-line-buffer-identification-keymap))

(defun uniline-position (&rest _)
  (propertize ":%l:%c "
              'face (uniline--face 'uniline-position-face)))

(defun uniline-flycheck ()
  "Return the status of flycheck to be displayed in the mode-line."
  (when (and (fboundp 'flycheck-mode) flycheck-mode)
    (let* ((text (pcase flycheck-last-status-change
                   (`finished (if flycheck-current-errors
                                  (let ((count (let-alist (flycheck-count-errors flycheck-current-errors)
                                                 (+ (or .warning 0) (or .error 0)))))
                                    (propertize (format "✖ %s Issue%s" count (if (eq 1 count) "" "s"))
                                                'face (uniline--face 'uniline-error-face)))
                                (propertize "✔ No Issues"
                                            'face (uniline--face 'uniline-ok-face))))
                   (`running     (propertize "⟲ Running"
                                             'face (uniline--face 'uniline-warning-face)))
                   (`not-checked "✖ Disabled")
                   (`errored     (propertize "⚠ Error"
                                             'face (uniline--face 'uniline-error-face)))
                   (`interrupted (propertize "⛔ Interrupted"
                                             'face (uniline--face 'uniline-error-face)))
                   (`suspicious  ""))))
      (propertize text
                  'help-echo "Show Flycheck Errors"
                  'local-map (make-mode-line-mouse-map
                              'mouse-1 #'flycheck-list-errors)))))


(defun uniline-evil (&rest _)
  (when (and (fboundp 'evil-mode)
             evil-mode)
    (unless (bound-and-true-p anzu--state)
      evil-mode-line-tag)))

(defun uniline-ro ()
  (when buffer-read-only
    (concat
     (uniline-spc)
     (propertize "RO" 'face (uniline--face 'uniline-ro-face)))))

;; `anzu' and `evil-anzu' expose current/total state that can be displayed in the
;; mode-line.
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

(defun uniline--anzu ()
  "Show the match index and total number thereof.
Requires `anzu', also `evil-anzu' if using `evil-mode' for compatibility with
`evil-search'."
  (when (and (bound-and-true-p anzu--state)
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
      'face (uniline--face 'uniline-panel))
     (uniline-spc))))

(defun uniline-misc (&rest _)
  (format-mode-line mode-line-misc-info))

(defun uniline-lsp (&rest _)
  "Update `lsp-mode' state."
  (when (fboundp 'lsp)
    (if-let* ((workspaces (lsp-workspaces))
              (face (if workspaces
                        'uniline
                      'uniline-warning-face)))
        (concat
         (propertize (mapconcat #'lsp--workspace-print lsp--buffer-workspaces "|")
                     'help-echo
                     (if workspaces
                         (concat "LSP Connected "
                                 (string-join
                                  (mapcar (lambda (w)
                                            (format "[%s]\n" (lsp--workspace-print w)))
                                          workspaces))
                                 "C-mouse-1: Switch to another workspace folder
mouse-1: Describe current session
mouse-2: Quit server
mouse-3: Reconnect to server")
                       "LSP Disconnected
mouse-1: Reload to start server")
                     'face face
                     'mouse-face 'uniline-highlight
                     'local-map (let ((map (make-sparse-keymap)))
                                  (if workspaces
                                      (progn
                                        (define-key map [mode-line C-mouse-1]
                                          #'lsp-workspace-folders-open)
                                        (define-key map [mode-line mouse-1]
                                          #'lsp-describe-session)
                                        (define-key map [mode-line mouse-2]
                                          #'lsp-workspace-shutdown)
                                        (define-key map [mode-line mouse-3]
                                          #'lsp-workspace-restart))
                                    (progn
                                      (define-key map [mode-line mouse-1]
                                        (lambda ()
                                          (interactive)
                                          (ignore-errors (revert-buffer t t))))))
                                  map))
         (uniline-spc)))))
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
        (require 'all-the-icons)

        (setq uniline--original-mode-line-format mode-line-format)

        (setq uniline--mode-line-format
              '(:eval (uniline--format
                       '(uniline-spc
                         uniline--anzu
                         uniline-evil
                         uniline-buffer-mark
                         uniline-buffer-name
                         uniline-position
                         uniline-misc)
                       '(uniline-flycheck
                         uniline-major-mode
                         uniline-lsp
                         uniline-encoding
                         uniline-ro))))

        (setq-default mode-line-format uniline--mode-line-format)
        (uniline--force-refresh uniline--mode-line-format))
    (progn
      ;; Reset the original modeline state
      (setq-default mode-line-format uniline--original-mode-line-format)
      (uniline--force-refresh uniline--original-mode-line-format)
      (setq uniline--original-mode-line-format nil))))

(provide 'uniline)
