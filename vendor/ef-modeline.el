(require 'all-the-icons)

(defgroup ef-modeline nil
  "A new, minimal mode-line."
  :group 'mode-line)

(defgroup ef-modeline-faces nil
  "The faces of `ef-modeline'."
  :group 'ef-modeline
  :group 'faces)

(defface ef-modeline
  '((t (:inherit mode-line)))
  "Face used for default."
  :group 'ef-modeline-faces)

(defface ef-modeline-inactive
  '((t (:inherit mode-line-inactive)))
  "Face used for inactive."
  :group 'ef-modeline-faces)

(defface ef-modeline-ro-face
  '((t :inherit ef-modeline :foreground "#0088CC"))
  "Face for read-only buffer in the mode-line.")

(defface ef-modeline-modified-face
  '((t :inherit ef-modeline :foreground "#ff6c6b"))
  "Face for modified buffers in the mode-line.")

(defface ef-modeline-not-modified-face
  '((t :inherit ef-modeline :foreground "#98be65"))
  "Face for not modified buffers in the mode-line.")

(defface ef-modeline-buffer-position-face
  '((t :inherit mode-line))
  "Face for line/column numbers in the mode-line.")

(defface ef-modeline-vc-face
  '((t :inherit ef-modeline :foreground "#61afef"))
  "Face for vc status in the mode-line.")

(defface ef-modeline-ok-face
  '((t :inherit ef-modeline :foreground "#61afef"))
  "Face for ok status in the mode-line.")

(defface ef-modeline-warning-face
  '((t :inherit ef-modeline :foreground "#da8548"))
  "Face for warning status in the mode-line.")

(defface ef-modeline-error-face
  '((t :inherit ef-modeline :foreground "#ff6c6b"))
  "Face for error status in the mode-line.")

;;
;; Flycheck defintions for byte-compiler
;;
(defvar flycheck-mode)
(defvar flycheck-last-status-change)
(defvar flycheck-current-errors)
(declare-function flycheck-count-errors "flycheck")
(declare-function flycheck-list-errors "flycheck")

;;
;; Minions definitions for byte-compiler
;;
(defvar minions-mode)
(defvar minions-mode-line-modes)

;;
;; Temp vars
;;
(defvar ef-modeline--original-mode-line-format)
(defvar ef-modeline--mode-line-format)

;;
;; Mode-line segments
;;
(defvar ef-modeline-buffer-identification '(:eval (propertize "%b" 'face 'bold))
  "Mode line construct for displaying the buffer name.")

(defvar ef-modeline-buffer-coding
  '(:eval (unless (eq buffer-file-coding-system (default-value 'buffer-file-coding-system))
            mode-line-mule-info)))

(defvar ef-modeline-modified
  '(:eval
    (if (buffer-file-name (current-buffer))
        (if (buffer-modified-p (current-buffer))
            (all-the-icons-octicon "file-code"
                                  :height 0.9
                                  :v-adjust 0
                                  :face (ef-modeline-face 'ef-modeline-modified-face))
          (all-the-icons-octicon "check"
                                :height 0.9
                                :v-adjust 0
                                :face (ef-modeline-face 'ef-modeline-not-modified-face)))
      " "
      )))

(defvar ef-modeline-position
  '(:eval (propertize ":%l:%c "
                      'face (ef-modeline-face 'ef-modeline-buffer-position-face)
                      'display '(min-width (7.0))))
  "Mode line construct for displaying the position in the buffer.")

;;
;; Keep `ef-modeline-current-window' up-to-date
;;
(defun ef-modeline--get-current-window (&optional frame)
  "Get the current window but should exclude the child windows.
If FRAME is nil, it means the current frame."
  (if (and (fboundp 'frame-parent) (frame-parent frame))
      (frame-selected-window (frame-parent frame))
    (frame-selected-window frame)))

(defvar ef-modeline-current-window (ef-modeline--get-current-window))

(defun ef-modeline--active ()
  "Whether is an active window."
  (unless (and (bound-and-true-p mini-frame-frame)
               (and (frame-live-p mini-frame-frame)
                    (frame-visible-p mini-frame-frame)))
    (and ef-modeline-current-window
         (eq (ef-modeline--get-current-window) ef-modeline-current-window))))

(defun ef-modeline-set-selected-window (&rest _)
  "Set `ef-modeline-current-window' appropriately."
  (let ((win (ef-modeline--get-current-window)))
    (setq ef-modeline-current-window
          (if (minibuffer-window-active-p win)
              (minibuffer-selected-window)
            win))))

(defun ef-modeline-unset-selected-window ()
  "Unset `ef-modeline-current-window' appropriately."
  (setq ef-modeline-current-window nil))

(add-hook 'pre-redisplay-functions #'ef-modeline-set-selected-window)

(defun ef-modeline-face (face &optional inactive-face)
  "Display FACE in mode-line.
If INACTIVE-FACE is nil, will use `mode-line-inactive' face."
  (if (ef-modeline--active)
      face
    (or inactive-face 'mode-line-inactive)))

;;
;; Flycheck
;;
(defun ef-modeline-flycheck-status ()
  "Return the status of flycheck to be displayed in the mode-line."
  (when (and (fboundp 'flycheck-mode) flycheck-mode)
    (let* ((text (pcase flycheck-last-status-change
                   (`finished (if flycheck-current-errors
                                  (let ((count (let-alist (flycheck-count-errors flycheck-current-errors)
                                                 (+ (or .warning 0) (or .error 0)))))
                                    (propertize (format "✖ %s Issue%s" count (if (eq 1 count) "" "s"))
                                                'face (ef-modeline-face 'ef-modeline-error-face)))
                                (propertize "✔ No Issues"
                                            'face (ef-modeline-face 'ef-modeline-ok-face))))
                   (`running     (propertize "⟲ Running"
                                             'face (ef-modeline-face 'ef-modeline-warning-face)))
                   (`no-checker  (propertize "⚠ No Checker"
                                             'face (ef-modeline-face 'ef-modeline-warning-face)))
                   (`not-checked "✖ Disabled")
                   (`errored     (propertize "⚠ Error"
                                             'face (ef-modeline-face 'ef-modeline-error-face)))
                   (`interrupted (propertize "⛔ Interrupted"
                                             'face (ef-modeline-face 'ef-modeline-error-face)))
                   (`suspicious  ""))))
      (propertize text
                  'help-echo "Show Flycheck Errors"
                  'local-map (make-mode-line-mouse-map
                              'mouse-1 #'flycheck-list-errors)))))

(defun ef-modeline-encoding ()
  "Displays the eol and the encoding style of the buffer."
  (let ((face (ef-modeline-face 'ef-modeline))
        (mouse-face 'ef-modeline))
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
          (0 "LF ")
          (1 "CRLF ")
          (2 "CR ")
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

;;
;; VC tracking
;;
(defvar-local ef-modeline--vcs-text nil)

(defun ef-modeline-update-vcs-text (&rest _)
  "Update text of vcs state in mode-line."
  (vc-refresh-state)
  (setq ef-modeline--vcs-text
        (when (and vc-mode buffer-file-name)
          (let* ((backend (vc-backend buffer-file-name))
                 (state (vc-state (file-local-name buffer-file-name) backend))
                 (str (if vc-display-status
                          (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                        "")))
            (propertize (if (length> str 25)
                            (concat
                             (substring str 0 (- 25 3))
                             "...")
                          str)
                        'face (cond ((eq state 'needs-update)
                                     'ef-modeline-warning-face)
                                    ((memq state '(removed conflict unregistered))
                                     'ef-modeline-error-face)
                                    ((memq state '(edited added))
                                     'ef-modeline-ok-face)
                                    (t 'ef-modeline-not-modified-face)))))))

(add-hook 'window-configuration-change-hook #'ef-modeline-update-vcs-text)
(add-hook 'find-file-hook #'ef-modeline-update-vcs-text)
(add-hook 'after-save-hook #'ef-modeline-update-vcs-text)

(defvar-local ef-modeline--vcs-icon nil)
(defun ef-modeline-update-vcs-icon (&rest _)
  "Update icon of vcs state in mode-line."
  (vc-refresh-state)
  (setq ef-modeline--vcs-icon
        (when (and vc-mode buffer-file-name)
          (let* ((backend (vc-backend buffer-file-name))
                 (state   (vc-state (file-local-name buffer-file-name) backend)))
            (cond ((memq state '(edited added))
                   (all-the-icons-octicon "git-compare"
                                          :height 0.9
                                          :v-adjust 0
                                          :face (ef-modeline-face 'ef-modeline-ok-face)))
                  ((eq state 'needs-merge)
                   (all-the-icons-octicon "git-merge"
                                          :height 0.9
                                          :v-adjust 0
                                          :face (ef-modeline-face 'ef-modeline-ok-face)))
                  ((eq state 'needs-update)
                   (all-the-icons-octicon "arrow-down"
                                          :height 0.9
                                          :v-adjust 0
                                          :face (ef-modeline-face 'ef-modeline-warning-face)))
                  ((memq state '(removed conflict unregistered))
                   (all-the-icons-octicon "alert"
                                          :height 0.9
                                          :v-adjust 0
                                          :face (ef-modeline-face 'ef-modeline-error-face)))
                  (t
                   (all-the-icons-octicon "git-branch"
                                          :height 0.9
                                          :v-adjust 0
                                          :face (ef-modeline-face 'ef-modeline-not-modified-face))))))))


(add-hook 'window-configuration-change-hook #'ef-modeline-update-vcs-icon)
(add-hook 'find-file-hook #'ef-modeline-update-vcs-icon)
(add-hook 'after-save-hook #'ef-modeline-update-vcs-icon)

(defun ef-modeline--force-refresh (format)
  "Updates the modeline format in each buffer."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (setq mode-line-format format))))

(defun ef-modeline--modes ()
  (if minions-mode
      minions-mode-line-modes
    mode-line-modes))

(define-minor-mode ef-modeline-mode
  "Toggle `ef-modeline' on or off."
  :group 'ef-modeline
  :global t
  :lighter nil
  (if ef-modeline-mode
      (progn
        (require 'all-the-icons)
        (setq ef-modeline--original-mode-line-format mode-line-format)
        (setq ef-modeline--mode-line-format
              `(" "
                (:eval (ef-modeline-encoding))
                " "
                " "
                ,ef-modeline-modified
                " "
                ,ef-modeline-buffer-identification
                ,ef-modeline-position
                (:eval evil-mode-line-tag)
                " "
                " "
                (:eval ef-modeline--vcs-icon) " "
                (:eval ef-modeline--vcs-text) " "
                "  "
                (:eval (ef-modeline--modes))
                " "
                (:eval (ef-modeline-flycheck-status))
                "  "
                mode-line-misc-info
                mode-line-end-spaces))
        (setq-default mode-line-format ef-modeline--mode-line-format)
        (ef-modeline--force-refresh ef-modeline--mode-line-format))
    (progn
      ;; Reset the original modeline state
      (setq-default mode-line-format ef-modeline--original-mode-line-format)
      (ef-modeline--force-refresh ef-modeline--original-mode-line-format)
      (setq ef-modeline--original-mode-line-format nil))))

(provide 'ef-modeline)
