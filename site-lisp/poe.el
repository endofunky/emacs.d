;;; poe.el --- A popup manager -*- lexical-binding: t; -*-
(require 'cl-extra)
(require 'cl-seq)

(defgroup poe nil
  "A window and popup manager."
  :group 'convenience)

(defvar poe-rules nil)

(defvar poe-popup-default-rule
  '(:side bottom
    :size 0.2))

(defun poe--plist-merge (&rest plists)
  "Create a single property list from all plists in PLISTS.
The process starts by copying the first list, and then setting
properties from the other lists.  Settings in the last list
are the most significant ones and overrule settings in the
other lists.

This is coming from `org-mode' (`org-combine-plists'). Requiring
`org-mode' loads a 24k+ line Emacs Lisp file, which introduces
significant overhead when used as a utility library, hence it
has been extracted."
  (let ((rtn (copy-sequence (pop plists)))
        p v ls)
    (while plists
      (setq ls (pop plists))
      (while ls
        (setq p (pop ls) v (pop ls))
        (setq rtn (plist-put rtn p v))))
    rtn))

(defun poe--alist (_alist rule)
  ;; If this is a popup, merge the default popup rules.
  ;;
  ;; Most of the time we want a panel of the bottom of the screen, so let's not
  ;; require specifying those rules ever single time we're declaring a popup.
  (let ((rule (if (plist-get rule :popupe)
                  (poe--plist-merge poe-popup-default-rule
                                    rule)
                rule)))
    `((actions         . ,(plist-get rule :actions))
      (side            . ,(plist-get rule :side))
      (size            . ,(plist-get rule :size))
      (slot            . ,(plist-get rule :slot)))))

(defun poe--match (buffer-or-name)
  (when-let* ((buffer (get-buffer buffer-or-name))
              (buffer-major-mode (buffer-local-value 'major-mode buffer))
              (buffer-name (buffer-name buffer)))
    (cl-loop
     for (condition . plist) in poe-rules
     when (or (and (symbolp condition)
                   (eq condition buffer-major-mode))
              (and (stringp condition)
                   (or (string= condition buffer-name)
                       (and (plist-get plist :regexp)
                            (string-match condition
                                          buffer-name)))))
     return plist)))

(defun poe--popup-split-window (window size side)
  "Ensure a non-dedicated/popup window is selected when splitting
a window."
  (cl-loop for win
           in (cons (or window (selected-window))
                    (window-list nil 0 window))
           unless (poe--popup-window-p win)
           return (setq window win))
  (let ((ignore-window-parameters t))
    (split-window window size side)))

(defun poe--popup-kill-buffer (buffer)
  (let ((inhibit-quit t))
    (cond
     ;; Buffer isn't live anymore, no need to kill it.
     ((not (buffer-live-p buffer)))
     ((not (get-buffer-window buffer t))
      (with-demoted-errors "Error killing transient buffer: %s"
        (with-current-buffer buffer
          (let (confirm-kill-processes)
            ;; Don't ask to kill processes.
            (when-let (process (get-buffer-process buffer))
              (when (eq (process-type process) 'real)
                (kill-process process)))
            (let (kill-buffer-query-functions)
              ;; HACK The debugger backtrace buffer, when killed, called
              ;;      `top-level'. This causes jumpiness when the popup
              ;;      manager tries to clean it up.
              (cl-letf (((symbol-function #'top-level) #'ignore))
                (kill-buffer buffer))))))))))

(defvar poe--popup-inhibit-kill-buffer nil)

(defun poe--popup-delete-window (window)
  (let ((buffer (window-buffer window))
        (inhibit-quit t))
    ;; If the window buffer is file-backed and has been modified, ask if we
    ;; want to save it.
    (and (or (buffer-file-name buffer)
             (if-let (base-buffer (buffer-base-buffer buffer))
                 (buffer-file-name base-buffer)))
         (buffer-modified-p buffer)
         (y-or-n-p "Popup buffer is modified. Save it?")
         (with-current-buffer buffer (save-buffer)))
    ;; Delete window or restore window configuration.
    (let ((ignore-window-parameters t))
      (if-let (wconf (window-parameter window 'saved-wconf))
          (set-window-configuration wconf)
        (delete-window window)))
    ;; Kill the buffer unless it's still a live-buffer.
    (unless (window-live-p window)
      (with-current-buffer buffer
        (set-buffer-modified-p nil)
        (poe-popup-mode -1)
        (when (and (not poe--popup-inhibit-kill-buffer)
                   (plist-get (window-parameter window 'poe-rule) :ephemeral))
          (poe--popup-kill-buffer buffer))))))

(defun poe--display-buffer (buffer alist rule)
  (let ((alist (poe--alist alist rule))
        (actions (or (cdr (assq 'actions alist))
                     ;; Use same window if :same is set, unless it's a popup
                     ;; window, in which case displaying it in the same window
                     ;; doesn't make much sense and showing a popup as
                     ;; expected in a side-window takes precedence.
                     (if (and (plist-get rule :same)
                              (not (plist-get rule :popup)))
                         '(display-buffer-reuse-window
                           display-buffer-same-window)
                       '(display-buffer-reuse-window
                         display-buffer-in-side-window)))))
    ;; Call all the display-buffer actions until we find one that works.
    (when-let (window (cl-loop for func in actions
                               if (funcall func buffer alist)
                               return it))
      (when (plist-get rule :popup)
        (with-selected-window window
          (set-window-parameter window 'split-window #'poe--popup-split-window)
          (set-window-parameter window 'delete-window #'poe--popup-delete-window)
          (set-window-parameter window 'poe-rule rule)
          (set-window-dedicated-p window 'popup)
          (poe-popup-mode t)))
      window)))

(defun poe--display-buffer-condition (buffer _action)
  (poe--match buffer))

(defun poe--display-buffer-action (buffer alist)
  (poe--display-buffer buffer alist (poe--match buffer)))

(defun poe--popup-buffer-p (&optional buffer)
  (let ((buffer (or buffer
                    (current-buffer))))
    (and (bufferp buffer)
         (buffer-live-p buffer)
         (buffer-local-value 'poe-popup-mode buffer)
         buffer)))

(defun poe--popup-window-p (&optional window)
  (poe--popup-buffer-p (window-buffer (or window
                                          (selected-window)))))

;; ----------------------------------------------------------------------------
;; Public
;; ----------------------------------------------------------------------------

(defmacro poe-rule (key &rest plist)
  (declare (indent 0))
  `(progn
     ;; Avoid having duplicate rules for a condition.
     (setq poe-rules (cl-delete ,key poe-rules :key #'car :test #'equal))
     (push '(,key ,@plist) poe-rules)))

;; ----------------------------------------------------------------------------
;; Modes
;; ----------------------------------------------------------------------------

(defvar poe-popup-mode-map (make-sparse-keymap)
  "Active keymap in `poe-mode' popup windows.

See `poe-popup-mode'.")

(define-minor-mode poe-popup-mode
  "Minor mode for individual `poe-mode' buffers to be shown in
popup windows."
  :group 'poe
  :lighter nil
  :keymap poe-popup-mode-map)

(defvar poe-mode-map (make-sparse-keymap)
  "Global keymap for `poe-mode'.")

(define-minor-mode poe-mode
  "Toggle `poe' on or off."
  :group 'poe
  :global t
  :lighter nil
  :keymap poe-mode-map
  (if poe-mode
      (setq display-buffer-alist
            (cons '(poe--display-buffer-condition poe--display-buffer-action)
                  display-buffer-alist))
    (setq display-buffer-alist
          (remove '(poe--display-buffer-condition poe--display-buffer-action)
                  display-buffer-alist))))

(provide 'poe)
