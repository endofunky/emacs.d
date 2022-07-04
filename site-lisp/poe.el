;;; poe.el --- A popup manager -*- lexical-binding: t; -*-
(require 'cl-extra)
(require 'cl-seq)

(defgroup poe nil
  "A window and popup manager."
  :group 'convenience)

(defcustom poe-popup-slot -1911
  "Slot number to use for popup windows."
  :group 'poe
  :type 'integer)

(defcustom poe-remove-fringes-from-popups t
  "Remove fringes from popup windows."
  :group 'poe
  :type 'boolean)

(defcustom poe-dim-popups t
  "Change (\"dim\") popup windows with a different background
color.

See: `poe-popup-dimmed-face'."
  :group 'poe
  :type 'boolean)

(defface poe-popup-dimmed-face
  `((t (:background "#151617")))
  "Face used for default."
  :group 'poe)

(defconst poe-rules-custom-type
  '(plist :options
          (((const :tag "Regexp" :regexp) boolean)
           ((const :tag "Same" :same) boolean)
           ((const :tag "Popup" :popup) boolean)
           ((const :tag "Select" :select) boolean)
           ((const :tag "Side" :side)
            (choice :tag "Alignment" :value nil
                    (const :tag "Default" nil)
                    (const :tag "Top" top)
                    (const :tag "Bottom" bottom)
                    (const :tag "Left" left)
                    (const :tag "Right" right)
                    (function :tag "Function")))
           ((const :tag "Size" :size) number)))
  "Shared custom :type fields for rules.")

(defcustom poe-rules nil
  "Window display rules."
  :group 'poe
  :type '(alist :key-type (choice :tag "Condition"
                                  (symbol :tag "Major mode")
                                  (string :tag "Buffer name"))
                :value-type poe-rules-custom-type))

(defcustom poe-popup-default-rule '(:side bottom
                                    :size 0.2)
  "Default rules to include for popup windows.

Supported rules are the same as for `poe-rules', however, the
\":popup\" rule has no effect."
  :group 'poe
  :type poe-rules-custom-type)

(defvar poe--popup-buffer-list nil
  "List of popup buffers in the order they were opened in.

The `car' of this list will be the most recently visible popup.
Used for cycling popup buffers with `poe-popup-next' and
`poe-popup-prev'.")

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
  "Transforms a poe rule into an alist in the format expected by
display buffer actions."
  (let ((rule (if (plist-get rule :popup)
                  ;; If this is a popup, merge the default popup rules and set
                  ;; the fixed slot number.
                  ;;
                  ;; Most of the time we want a panel of the bottom of the
                  ;; screen, so let's not require specifying those rules ever
                  ;; single time we're declaring a popup.
                  (poe--plist-merge poe-popup-default-rule
                                    rule
                                    `(:slot ,poe-popup-slot))
                rule)))
    `((actions         . ,(plist-get rule :actions))
      (side            . ,(plist-get rule :side))
      (size            . ,(plist-get rule :size))
      (slot            . ,(plist-get rule :slot)))))

(defun poe--match (buffer-or-name)
  "Match BUFFER-OR-NAME against any conditions defined in
`poe-rules' and, if found, returns the rule plist"
  (when-let* ((buffer (get-buffer buffer-or-name))
              (buffer-major-mode (buffer-local-value 'major-mode buffer))
              (buffer-name (buffer-name buffer)))
    (cl-loop
     for (condition . plist) in poe-rules
     when (or
           ;; Symbol, compare to major-mode
           (and (symbolp condition)
                (eq condition buffer-major-mode))
           ;; String, compare to buffer name or match against it if the :regexp
           ;; rule is set.
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
  "Kill the popup-buffer BUFFER after killing any associated
buffer-process. "
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

(defvar poe--popup-inhibit-kill-buffer nil
  "Variable that can be set during a delete-window cycle to inhibit
any `kill-buffer' calls issued for buffers with the :ephemeral
rule.")

(defvar poe--popup-force-kill-buffer nil
  "Variable that can be set during a delete-window cycle to force
a subsequent `kill-buffer', irrespective of the :ephemeral rule.")

(defun poe--popup-delete-window (window)
  "A `delete-window' window-parameter function for popup buffers.

Will ask to save if the popup is file backed and modified, restore the
window-configuration and kill the popup buffer if it's marked as :ephemeral,
unless the window is still visiable after restoring the previous window
configuration."
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
        (when (or poe--popup-force-kill-buffer
                  (and (not poe--popup-inhibit-kill-buffer)
                       (plist-get (window-parameter window 'poe-rule) :ephemeral)))
          (setq poe--popup-buffer-list (remove buffer poe--popup-buffer-list))
          (poe--popup-kill-buffer buffer))))))

(defun poe--display-buffer (buffer alist rule)
  "Handles displaying of poe-managed buffers, optionally opening them in a
popup-window."
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
      (when (plist-get rule :select)
        (select-window window))
      window)))

(defun poe--display-buffer-condition (buffer _action)
  "Condition function for `display-buffer-alist'."
  (poe--match buffer))

(defun poe--display-buffer-action (buffer alist)
  "Action function for `display-buffer-alist'."
  (poe--display-buffer buffer alist (poe--match buffer)))

(defun poe--popup-buffer-p (&optional buffer)
  "Return BUFFER if the bufferis a popup buffer,`nil' otherwise.

Defaults to the current buffer."
  (let ((buffer (or buffer
                    (current-buffer))))
    (and (bufferp buffer)
         (buffer-live-p buffer)
         (buffer-local-value 'poe-popup-mode buffer)
         buffer)))

(defun poe--popup-window-p (&optional window)
  "Return WINDOW if the window's buffer is a popup buffer, `nil'
otherwise.

Defaults to the currently selected window."
  (let ((window (or window (selected-window))))
    (and (poe--popup-buffer-p (window-buffer window))
         window)))

(defun poe--move-to-front (elt list)
  "Add/mode ELT to the front of LIST."
  (cons elt (remove elt list)))

(defun poe--popup-buffers ()
  "Returns a list of open popup buffers."
  (seq-filter #'poe--popup-buffer-p (buffer-list)))

(defun poe--popup-windows ()
  "Returns a list of open popup windows."
  (seq-filter #'poe--popup-window-p (window-list)))

(defun poe--popup-update-buffer-list-h ()
  "Function called from `window-configuration-change-hook' to update
`ef-popup--buffer-list' with any changes."
  (dolist (buf (cl-set-difference (poe--popup-buffers)
                                  poe--popup-buffer-list))
    (setq poe--popup-buffer-list
          (poe--move-to-front buf poe--popup-buffer-list))))

(defun poe--popup-dim-h ()
  (when poe-dim-popups
    (if (poe--popup-buffer-p)
        (buffer-face-set
         `(:background ,(face-background 'poe-popup-dimmed-face)))
      (buffer-face-set 'default))))

(defun poe--popup-remove-fringes-h ()
  (when poe-remove-fringes-from-popups
    (let ((f (if (poe--popup-buffer-p) 0)))
      (set-window-fringes nil f f fringes-outside-margins))))

;; ----------------------------------------------------------------------------
;; Public
;; ----------------------------------------------------------------------------

;;;###autoload
(defun poe-rule (key &rest plist)
  "Add a new display-buffer rule to `poe-rules'."
  ;; Avoid having duplicate rules for a condition.
  (setq poe-rules (cl-delete key poe-rules :key #'car :test #'equal))
  (push (cons key plist) poe-rules))

;;;###autoload
(defun poe-popup (key &rest plist)
  "Add a new display-buffer rule for a popup window to `poe-rules'."
  (setq poe-rules (cl-delete key poe-rules :key #'car :test #'equal))
  (push (cons key (plist-put plist :popup t)) poe-rules))

;;;###autoload
(defun poe-popup-discard ()
  "Kills the currently visible popup, if any. "
  (interactive)
  (when-let ((win (car (poe--popup-windows))))
    (let ((poe--popup-force-kill-buffer t))
      (delete-window win))))

;;;###autoload
(defun poe-popup-kill ()
  "Kills the currently visible popup, if any.

If there is more than one buffer in the buffer-list that is managed
as a popup, display that buffer."
  (interactive)
  (when-let ((win (car (poe--popup-windows))))
    (let ((poe--popup-force-kill-buffer t))
      (delete-window win))
    (when-let ((buf (car poe--popup-buffer-list)))
      (display-buffer buf))))

;;;###autoload
(defun poe-popup-toggle ()
  "Toggle visibility of the last opened popup window."
  (interactive)
  (if-let ((win (car (poe--popup-windows))))
      (delete-window win)
    (when-let ((buf (car poe--popup-buffer-list)))
      (display-buffer buf))))

;;;###autoload
(defun poe-popup-next ()
  "Cycle visibility of popup windows forwards."
  (interactive)
  (if (= 0 (length (poe--popup-windows)))
      (poe-popup-toggle)
    (when poe--popup-buffer-list
      (setq poe--popup-buffer-list
            (cons (car (last poe--popup-buffer-list))
                  (butlast poe--popup-buffer-list)))
      (display-buffer (car poe--popup-buffer-list)))))

;;;###autoload
(defun poe-popup-prev ()
  "Cycle visibility of popup windows backwards."
  (interactive)
  (if (= 0 (length (poe--popup-windows)))
      (poe-popup-toggle)
    (when poe--popup-buffer-list
      (setq poe--popup-buffer-list
            (append (cdr poe--popup-buffer-list)
                    (list (car poe--popup-buffer-list))))
      (display-buffer (car poe--popup-buffer-list)))))

;; ----------------------------------------------------------------------------
;; Modes
;; ----------------------------------------------------------------------------

(defvar poe-popup-mode-map (make-sparse-keymap)
  "Active keymap in `poe-mode' popup windows.

See `poe-popup-mode'.")

;;;###autoload
(define-minor-mode poe-popup-mode
  "Minor mode for individual `poe-mode' buffers to be shown in
popup windows."
  :group 'poe
  :lighter nil
  :keymap poe-popup-mode-map)

(defvar poe-mode-map (make-sparse-keymap)
  "Global keymap for `poe-mode'.")

;;;###autoload
(define-minor-mode poe-mode
  "Toggle `poe' on or off."
  :group 'poe
  :global t
  :lighter nil
  :keymap poe-mode-map
  (if poe-mode
      (progn
        (add-hook 'window-configuration-change-hook
                  #'poe--popup-update-buffer-list-h)
        (add-hook 'poe-popup-mode-hook #'poe--popup-dim-h)
        (add-hook 'poe-popup-mode-hook #'poe--popup-remove-fringes-h)
        (setq display-buffer-alist
              (cons '(poe--display-buffer-condition poe--display-buffer-action)
                    display-buffer-alist)))
    (remove-hook 'window-configuration-change-hook
                 #'poe--popup-update-buffer-list-h)
    (remove-hook 'poe-popup-mode-hook #'poe--popup-dim-h)
    (remove-hook 'poe-popup-mode-hook #'poe--popup-remove-fringes-h)
    (setq display-buffer-alist
          (remove '(poe--display-buffer-condition poe--display-buffer-action)
                  display-buffer-alist))))

(provide 'poe)
