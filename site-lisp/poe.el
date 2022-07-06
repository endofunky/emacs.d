;;; poe.el --- A popup & window manager -*- lexical-binding: t; -*-
;;
;; A popup and window management library, based in large parts on
;; shackle and doom emacs's popup module. It uses shackle's window
;; creation routines and combines those with some popup management
;; features from doom emacs.
;;
;; As opposed to doom popups, in poe there will only ever be one
;; popup shown at a time. To support a fast work flow, many
;; commands to deal with popup exist, such as cycling backwards
;; and forwards, "remote" killing of popups from other buffers,
;; and more.
;;
(require 'cl-extra)
(require 'cl-seq)

(defgroup poe nil
  "A window and popup manager."
  :group 'convenience)

(defcustom poe-popup-slot -1911
  "Slot number to use for popup windows."
  :group 'poe
  :type 'integer)

(defcustom poe-default-size 0.5
  "Default size of aligned windows.

A floating point number between 0 and 1 is interpreted as a
ratio.  An integer equal or greater than 1 is interpreted as a
number of lines. If a function is specified, it is called with
zero arguments and must return a number of the above two types."
  :type '(choice (integer :tag "Number of lines")
                 (float :tag "Number of lines (ratio)")
                 (function :tag "Custom"))
  :group 'poe)

(defcustom poe-default-alignment 'below
  "Default alignment of aligned windows.
It may be one of the following values:

above: Align above the currently selected window.

below: Align below the currently selected window.

left: Align on the left side of the currently selected window.

right: Align on the right side of the currently selected
window.

<function>: Call the specified function with no arguments to
determine side, must return one of the above four values."
  :type '(choice (const :tag "Above" above)
                 (const :tag "Below" below)
                 (const :tag "Left" left)
                 (const :tag "Right" right))
  :group 'poe)

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
           ((const :tag "Shrink" :shrink) boolean)
           ((const :tag "Inhibit window quit" :inhibit-window-quit) boolean)
           ((const :tag "Align" :align)
            (choice :tag "Align" :value nil
                    (const :tag "Default" nil)
                    (const :tag "Above" above)
                    (const :tag "Below" below)
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

(defcustom poe-default-rule nil
  "Default rules to include for all windows.

Supported rules are the same as for `poe-rules'."
  :group 'poe
  :type poe-rules-custom-type)

(defcustom poe-popup-default-rule '(:align below
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

(defun poe--alist-merge (&rest alists)
  "Create a single association list from all alists in ALISTS.
The process starts by copying the first list, and then setting
associations from the other lists.  Settings in the last list
are the most significant ones and overrule settings in the
other lists.

Adapted from `org-combine-plists' for association lists."
  (let ((rtn (copy-sequence (pop alists)))
        ls)
    (while alists
      (setq ls (pop alists))
      (dolist (x ls)
        (if (null (assoc (car x) rtn))
            (setq rtn (nconc rtn (list x)))
          (setcdr (assq (car x) rtn) (cdr x)))))
    rtn))

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

(defun poe--transform-side (split-side)
  "Transforms SPLIT-SIDE argument for `split-window' into a
\"side\" suitable for for `display-buffer-alist' alists."
  (pcase split-side
    ('above 'top)
    ('below 'bottom)
    (_ split-side)))

(defun poe--alist (alist rule)
  "Transforms a poe rule into an alist in the format expected by
display buffer actions."
  (poe--alist-merge
   alist
   `((actions         . ,(plist-get rule :actions))
     (size            . ,(plist-get rule :size))
     (side            . ,(poe--transform-side (plist-get rule :size)))
     (slot            . ,(plist-get rule :slot)))))

(defun poe--match (buffer-or-name)
  "Match BUFFER-OR-NAME against any conditions defined in
`poe-rules' and, if found, returns the rule plist."
  (when-let* ((buffer (get-buffer buffer-or-name))
              (buffer-major-mode (buffer-local-value 'major-mode buffer))
              (buffer-name (buffer-name buffer)))
    (cl-loop
     for (condition . plist) in poe-rules
     when (or
           ;; Symbol, compare to major-mode
           (and (symbolp condition)
                (eq condition buffer-major-mode))
           ;; String, compare to buffer name or match against it if
           ;; the :regexp rule is set.
           (and (stringp condition)
                (or (string= condition buffer-name)
                    (and (plist-get plist :regexp)
                         (string-match condition
                                       buffer-name)))))
     return plist)))

(defun poe--popup-match (buffer-or-name)
  "Match BUFFER-OR-NAME against any conditions defined in
`poe-rules' and, if found and a popup, returns the rule plist."
  (let ((rule (poe--match buffer-or-name)))
    (when (plist-get rule :popup)
      rule)))

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

(defun poe--popup-remove-from-list (&optional buffer)
  "Remove BUFFER from the popup buffer list.

Defaults to the current buffer."
  (let ((buffer (or buffer
                    (current-buffer))))
    (setq poe--popup-buffer-list
          (remove buffer poe--popup-buffer-list))))

(defvar poe--popup-inhibit-select-buffer nil
  "Variable that can be set to inhibit `display-buffer' to select
a new popup that has the :select rule when the popup buffer is not
already focused.")

(defvar poe--popup-inhibit-kill-buffer nil
  "Variable that can be set during a delete-window cycle to inhibit
any `kill-buffer' calls issued for buffers with the :ephemeral
rule.")

(defvar poe--popup-force-kill-buffer nil
  "Variable that can be set during a delete-window cycle to force
a subsequent `kill-buffer', irrespective of the :ephemeral rule.")

(defun poe--popup-kill-buffer (buffer)
  "Kill the popup-buffer BUFFER after killing any associated
buffer-process. "
  (let ((inhibit-quit t))
    (cond
     ;; Buffer isn't live anymore, no need to kill it.
     ((not (buffer-live-p buffer)))
     ((not (get-buffer-window buffer t))
      (with-demoted-errors "Error killing ephemeral buffer: %s"
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

(defun poe--popup-delete-window (window)
  "A `delete-window' window-parameter function for popup buffers.

Will ask to save if the popup is file backed and modified, restore
the window-configuration and kill the popup buffer if it's marked
as :ephemeral,unless the window is still visiable after restoring
the previous window configuration."
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
    ;; Delete window.
    (let ((ignore-window-parameters t))
      (delete-window window))
    ;; Kill the buffer unless it's still a live-buffer.
    (unless (window-live-p window)
      (with-current-buffer buffer
        (set-buffer-modified-p nil)
        (poe-popup-mode -1)
        (when (or poe--popup-force-kill-buffer
                  (and (not poe--popup-inhibit-kill-buffer)
                       (plist-get (window-parameter window 'poe-rule)
                                  :ephemeral)))
          (poe--popup-remove-from-list buffer)
          (poe--popup-kill-buffer buffer))))))

(defun poe--frame-splittable-p (frame)
  "Return FRAME if it is splittable."
  (when (and (window--frame-usable-p frame)
             (not (frame-parameter frame 'unsplittable)))
    frame))

(defun poe--splittable-frame ()
  "Return a splittable frame to work on.

This can be either the selected frame or the last frame that's
not displaying a lone minibuffer."
  (let ((selected-frame (selected-frame))
        (last-non-minibuffer-frame (last-nonminibuffer-frame)))
    (or (poe--frame-splittable-p selected-frame)
        (poe--frame-splittable-p last-non-minibuffer-frame))))

(defun poe--find-window-by-slot (slot)
  "Find window with window-parameter \"window-slot\" matching SLOT.

If found, returns the existing window."
  (when slot
    (let ((this-slot)
          (this-window))
      (catch 'found
        (dolist (window (window-list))
          (setq this-slot (window-parameter window 'window-slot))
          (cond
           ((not (numberp this-slot)))
           ((= this-slot slot)
	    ;; A window with a matching slot has been found.
	    (setq this-window window)
	    (throw 'found t)))))
      this-window)))

(defun poe--display-buffer-aligned-window (buffer alist plist)
  "Display BUFFER in an aligned window.

ALIST is passed to `window--display-buffer' internally.
Optionally use a different alignment and/or size if PLIST
contains the :alignment key with an alignment different than the
default one in `poe-default-alignment' and/or PLIST contains
the :size key with a number value."
  (let ((frame (poe--splittable-frame)))
    (when frame
      (let* ((alignment-argument (plist-get plist :align))
             (alignments '(above below left right))
             (alignment (cond
                         ((functionp alignment-argument)
                          (funcall alignment-argument))
                         ((memq alignment-argument alignments)
                          alignment-argument)
                         ((functionp poe-default-alignment)
                          (funcall poe-default-alignment))
                         (t poe-default-alignment)))
             (horizontal (when (memq alignment '(left right)) t))
             (old-size (window-size (frame-root-window) horizontal))
             (size (or (plist-get plist :size)
                       (if (functionp poe-default-size)
                           (funcall poe-default-size)
                         poe-default-size)))
             (new-size (round (if (>= size 1)
                                  (- old-size size)
                                (* (- 1 size) old-size)))))
        (if (or (< new-size (if horizontal window-min-width window-min-height))
                (> new-size (- old-size (if horizontal window-min-width
                                          window-min-height))))
            (error "Invalid aligned window size %s, aborting" new-size)
          (let ((slot (plist-get plist :slot)))
            ;; Ok, this is a nasty work-around. If we have a window with a
            ;; matching slot and try to reuse the window, we have to deal with
            ;; calculating deltas to resize. Also, the window may not be
            ;; resizable. So in the end it's easier to just delete the window
            ;; and create a new one so popups also end up where they're
            ;; expected.
            (when-let ((existing-window (poe--find-window-by-slot slot)))
              ;; Edge case: In some cases it may not be safe to delete the
              ;; window, so we just ignore that one already exists and spawn a
              ;; new one anyway.
              (when (window-deletable-p existing-window)
                (delete-window existing-window)))
            (let ((window (split-window (frame-root-window frame)
                                        new-size alignment)))
              (prog1 (window--display-buffer buffer window 'window alist)
                (set-window-parameter window 'window-slot slot)
                (unless (cdr (assq 'inhibit-switch-frame alist))
                  (window--maybe-raise-frame frame))))))))))

(defun poe--display-buffer-reuse-window (buffer alist _plist)
  "Wrapper for `display-buffer-reuse-window' for `poe-mode'."
  (display-buffer-reuse-window buffer alist))

(defun poe--display-buffer-same-window (buffer alist _plist)
  "Wrapper for `display-buffer-same-window' for `poe-mode'."
  (display-buffer-same-window buffer alist))

(defun poe--display-buffer-use-some-window (buffer alist _plist)
  "Wrapper for `display-buffer-use-some-window' for `poe-mode'."
  (display-buffer-use-some-window buffer alist))

(defun poe--popup-delete-other-windows (&rest _)
  "Handler for `delete-other-windows' window-parameter for
poe-popup-mode buffers."
  (error "Cannot make popup window the only window"))

(defun poe--display-buffer-actions (alist rule)
  "Return display-buffer actions to be used for displaying a buffer
with ALIST and poe rules RULE."
  (or (cdr (assq 'actions alist))
      (cond
       ;; Use same window if :same is set, unless it's a popup window, in which
       ;; case displaying it in the same window doesn't make much sense and
       ;; showing a popup as expected in an aligned-window takes precedence.
       ((and (plist-get rule :same)
             (and (not (plist-get rule :popup))
                  ;; There is `display-buffer--same-window-action' which things
                  ;; like `info' use to reuse the currently selected window, it
                  ;; happens to be of the (inhibit-same-window . nil) form and
                  ;; should be permitted unless a popup is requested
                  (and (assq 'inhibit-same-window alist)
                       (not (cdr (assq 'inhibit-same-window alist))))))
        '(poe--display-buffer-reuse-window
          poe--display-buffer-same-window))
       ;; When :popup or :side is set, display it as an aligned window.
       ((or (plist-get rule :popup)
            (plist-get rule :side))
        '(poe--display-buffer-reuse-window
          poe--display-buffer-aligned-window))
       ;; Default to `display-buffer-use-some-window'.
       (t
        '(poe--display-buffer-reuse-window
          poe--display-buffer-use-some-window)))))

(defun poe--init-popup (window)
  "Initialize `poe-popup-mode' and popup-specific
window-parameters for WINDOW."
  (with-selected-window window
    (set-window-dedicated-p window t)
    (set-window-parameter window
                          'split-window
                          #'poe--popup-split-window)
    (set-window-parameter window
                          'delete-window
                          #'poe--popup-delete-window)
    (set-window-parameter window
                          'delete-other-windows
                          #'poe--popup-delete-other-windows)
    (set-window-dedicated-p window 'popup)
    (poe-popup-mode t)))

(defun poe--normalize-rule (rule)
  "Normalize RULE by merging with `poe-default-rule'.

If rule has :popup set to t, will also merge RULE with
`poe-popup-default-rule' and force the slot to `poe-popup-slot'."
  (if (plist-get rule :popup)
      (poe--plist-merge poe-default-rule
                        poe-popup-default-rule
                        rule
                        `(:slot ,poe-popup-slot))
    (poe--plist-merge poe-default-rule
                      rule)))

(defun poe--display-buffer (buffer alist rule)
  "Handles displaying of poe-managed buffers."
  (let* ((origin (selected-window))
         (origin-was-popup (poe--popup-window-p origin))
         (rule (poe--normalize-rule rule))
         (alist (poe--alist alist rule))
         (actions (poe--display-buffer-actions alist rule)))
    ;; Call all the display-buffer actions until we find one that works.
    (when-let (window (cl-loop for func in actions
                               if (funcall func buffer alist rule)
                               return it))
      (set-window-parameter window 'poe-rule rule)
      (when (plist-get rule :inhibit-window-quit)
        (set-window-parameter window 'quit-restore nil))
      (when (plist-get rule :popup)
        (poe--init-popup window))
      (when (plist-get rule :shrink)
        (poe--shrink-to-fit window))
      ;; Select the new popup if the :select rule is set. When the origin
      ;; window was a popup, mainain focus irrespective of :select.
      (if (or origin-was-popup
              (and (not poe--popup-inhibit-select-buffer)
                   (plist-get rule :select)))
          (select-window window)
        ;; Handle edge case where we spawn a popup without selecting it and
        ;; the currently highlighted line in `hl-line-mode' buffers with
        ;; `hl-line-sticky-flag' set to nil would disappear until the cursor
        ;; is moved.
        (with-selected-window origin
          (when (and (bound-and-true-p hl-line-mode)
                     (fboundp 'hl-line-highlight))
            (hl-line-highlight))))
      window)))

(defun poe--display-buffer-condition (buffer _action)
  "Condition function for `display-buffer-alist'."
  (poe--match buffer))

(defun poe--display-buffer-action (buffer alist)
  "Action function for `display-buffer-alist'."
  (poe--display-buffer buffer alist (poe--match buffer)))

(defun poe--shrink-to-fit (&optional window)
  "Shrinks WINDOW to fit the buffer contents, if the buffer isn't empty.
Uses `shrink-window-if-larger-than-buffer'."
  (unless window
    (setq window (selected-window)))
  (unless (= (- (point-max) (point-min)) 0)
    (shrink-window-if-larger-than-buffer window)))

(defun poe--popup-buffer-p (&optional buffer)
  "Return BUFFER if the buffer is a popup buffer, `nil' otherwise.

Defaults to the current buffer."
  (let ((buffer (or buffer
                    (current-buffer))))
    (and (bufferp buffer)
         (buffer-live-p buffer)
         (buffer-local-value 'poe-popup-mode buffer)
         buffer)))

(defun poe--regular-buffer-p (&optional buffer)
  "Return BUFFER if the buffer is a regular buffer, `nil' otherwise.

Defaults to the current buffer."
  (let ((buffer (or buffer
                    (current-buffer))))
    (when (not (poe--popup-buffer-p buffer))
      buffer)))

(defun poe--popup-window-p (&optional window)
  "Return WINDOW if the window's buffer is a popup buffer, `nil'
otherwise.

Defaults to the currently selected window."
  (let ((window (or window (selected-window))))
    (and (poe--popup-buffer-p (window-buffer window))
         window)))

(defsubst poe--move-to-front (elt list)
  "Add/mode ELT to the front of LIST."
  (cons elt (remove elt list)))

(defun poe--popup-all-buffers ()
  "Returns a list of open popup buffers."
  (seq-filter #'poe--popup-match (buffer-list)))

(defun poe--popup-buffers ()
  "Returns a list of open popup buffers."
  (seq-filter #'poe--popup-buffer-p (buffer-list)))

(defun poe--popup-windows ()
  "Returns a list of open popup windows."
  (seq-filter #'poe--popup-window-p (window-list)))

;; ----------------------------------------------------------------------------
;; Advice functions
;; ----------------------------------------------------------------------------

(defun poe--switch-to-buffer-obey-display-actions-a (orig-fun &rest args)
  "Advice to make `switch-to-buffer' obey `display-buffer-alist'.

Works by let-binding `switch-to-buffer-obey-display-actions' to t."
  (let ((switch-to-buffer-obey-display-actions t))
    (apply orig-fun args)))

;; ----------------------------------------------------------------------------
;; Hook functions
;; ----------------------------------------------------------------------------

(defun poe--popup-update-buffer-list-h ()
  "Hook function called from `window-configuration-change-hook' to
update `ef-popup--buffer-list' with any changes."
  (dolist (buf (cl-set-difference (poe--popup-all-buffers)
                                  poe--popup-buffer-list))
    (setq poe--popup-buffer-list
          (poe--move-to-front buf poe--popup-buffer-list))))

(defun poe--popup-dim-h ()
  "Hook function to dim buffer background with the :background
property from `poe-popup-dimmed-face'.

Called in `poe-popup-buffer-mode-hook'"
  (when (and (bound-and-true-p poe-popup-mode)
             poe-dim-popups)
    (if (poe--popup-buffer-p)
        (buffer-face-set
         `(:background ,(face-background 'poe-popup-dimmed-face)))
      (buffer-face-set 'default))))

(defun poe--popup-remove-fringes-h ()
  "Hook function to remove finges from popup buffers.

Called in `poe-popup-buffer-mode-hook'"
  (when (and (bound-and-true-p poe-popup-mode)
             poe-remove-fringes-from-popups)
    (let ((f (if (poe--popup-buffer-p) 0)))
      (set-window-fringes nil f f fringes-outside-margins))))

(defun poe--popup-run-hooks-maybe-h ()
  "Hook function called from `after-change-major-mode-hook' to re-run
`poe-popup-mode-hook' hooks if the buffer still matches the conditions.

Some packages set major-modes after displaying the window, which will disable
`poe-popup-mode' again. While we keep the variable `poe-popup-mode' as
permanent-local, we have to re-run the hooks again."
  (when (and (bound-and-true-p poe-mode)
             (bound-and-true-p poe-popup-mode)
             (poe--popup-match (current-buffer)))
    (run-hooks 'poe-popup-mode-hook)))

;; ----------------------------------------------------------------------------
;; Public
;; ----------------------------------------------------------------------------

;;;###autoload
(defun poe-popup-save-a (orig-fun &rest args)
  "Sets aside popups before executing the original function, usually to
prevent the popup(s) from messing up the UI (or vice versa)."
  (let ((poe--popup-inhibit-kill-buffer t))
    (if (poe--popup-windows)
        (prog2
            (poe-popup-toggle)
            (apply orig-fun args)
          (poe-popup-toggle))
      (apply orig-fun args))))

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
  "Toggle visibility of the last opened popup window.

Will select the popup window if the popup rule specifies :select."
  (interactive)
  (if-let ((win (car (poe--popup-windows))))
      ;; Popup window is open, so close it.
      (delete-window win)
    (if-let ((buf (car poe--popup-buffer-list)))
        ;; We have an active "session" of displaying popups, sodisplay the most
        ;; recent one.
        (display-buffer buf)
      ;; All popups have been closed, so find a buried popup window and display
      ;; it if we found one.
      (cl-loop for buf being the buffers
               if (let ((rule (poe--match buf)))
                    (plist-get rule :popup))
               do (display-buffer buf)
               and return it))))

;;;###autoload
(defun poe-popup-next ()
  "Cycle visibility of popup windows forwards."
  (interactive)
  (let ((poe--popup-inhibit-select-buffer t))
    (if (= 0 (length (poe--popup-windows)))
        (poe-popup-toggle)
      (when poe--popup-buffer-list
        (setq poe--popup-buffer-list
              (cons (car (last poe--popup-buffer-list))
                    (butlast poe--popup-buffer-list)))
        (display-buffer (car poe--popup-buffer-list))))))

;;;###autoload
(defun poe-popup-prev ()
  "Cycle visibility of popup windows backwards."
  (interactive)
  (let ((poe--popup-inhibit-select-buffer t))
    (if (= 0 (length (poe--popup-windows)))
        (poe-popup-toggle)
      (when poe--popup-buffer-list
        (setq poe--popup-buffer-list
              (append (cdr poe--popup-buffer-list)
                      (list (car poe--popup-buffer-list))))
        (display-buffer (car poe--popup-buffer-list))))))

;; ----------------------------------------------------------------------------
;; Consult source
;; ----------------------------------------------------------------------------

(defun poe--consult-source-predicate (buffer)
  "Predicate function for `consult'.

If the currently selected buffer is a popup, returns t if BUFFER is
a popup, otherwise returns t if BUFFER is a regular buffer. "
  (if (poe--popup-buffer-p)
      (when-let ((rule (poe--match buffer)))
        (plist-get rule :popup))
    (if-let ((rule (poe--match buffer)))
        (not (plist-get rule :popup))
      t)))

(defun poe--consult-source-query ()
  "Query function for `consult'.

Uses `poe--consult-source-predicate' to provide popup context
aware buffer lists."
  (consult--buffer-query :sort 'visibility
                         :predicate #'poe--consult-source-predicate
                         :as #'buffer-name))

(with-eval-after-load 'consult
  (declare-function consult--buffer-state "ext:consult")
  (declare-function consult--buffer-query "ext:consult")

  (defvar poe-consult-source
    (list :name     "Poe Buffers"
          :narrow   ?s
          :category 'buffer
          :state    #'consult--buffer-state
          :history  'buffer-name-history
          :default  t
          :items #'poe--consult-source-query)
    "A `consult' source for `poe-mode'.

Provides buffer lists based on the currently selected buffer. If a
popup is selected, will give a list of popups, otherwise gives a
list of regular, non-popup buffers."))

;; ----------------------------------------------------------------------------
;; Modes
;; ----------------------------------------------------------------------------

(defvar poe-popup-mode-map (make-sparse-keymap)
  "Active keymap in `poe-mode' popup windows.

See `poe-popup-mode'.")

;;;###autoload
(define-minor-mode poe-popup-mode
  "Minor mode for `poe-mode' buffers shown in popup windows."
  :group 'poe
  :lighter nil
  :interactive nil
  :init-value nil
  :keymap poe-popup-mode-map)

(put 'poe-popup-mode 'permanent-local t)
(put 'poe-popup-mode 'permanent-local-hook t)

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
      ;; Mode enabled
      (progn
        (add-hook 'after-change-major-mode-hook #'poe--popup-run-hooks-maybe-h)
        (add-hook 'window-configuration-change-hook
                  #'poe--popup-update-buffer-list-h)
        (add-hook 'kill-buffer-hook #'poe--popup-remove-from-list)
        (setq display-buffer-alist
              (cons '(poe--display-buffer-condition poe--display-buffer-action)
                    display-buffer-alist))
        (advice-add 'switch-to-buffer-other-window
                    :around #'poe--switch-to-buffer-obey-display-actions-a)
        (advice-add 'switch-to-buffer
                    :around #'poe--switch-to-buffer-obey-display-actions-a)
        (advice-add 'balance-windows :around #'poe-popup-save-a)
        (poe--popup-update-buffer-list-h))
    ;; Mode disabled
    (remove-hook 'after-change-major-mode-hook #'poe--popup-run-hooks-maybe-h)
    (remove-hook 'kill-buffer-hook #'poe--popup-remove-from-list)
    (remove-hook 'window-configuration-change-hook
                 #'poe--popup-update-buffer-list-h)
    (advice-remove 'switch-to-buffer-other-window
                   #'poe--switch-to-buffer-obey-display-actions-a)
    (advice-remove 'switch-to-buffer
                   #'poe--switch-to-buffer-obey-display-actions-a)
    (setq display-buffer-alist
          (remove '(poe--display-buffer-condition poe--display-buffer-action)
                  display-buffer-alist))
    (advice-remove 'balance-windows #'poe-popup-save-a)))

(add-hook 'poe-popup-mode-hook #'poe--popup-dim-h)
(add-hook 'poe-popup-mode-hook #'poe--popup-remove-fringes-h)

(provide 'poe)
