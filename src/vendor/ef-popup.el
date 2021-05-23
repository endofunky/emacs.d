(require 'cl-seq)
(require 'cl-macs)
(require 'shackle)
(require 'subr-x)

(defvar ef-popup--buffer-list '()
  "List of popup buffers in the order they were opened in.

The `car' of this list will be the most recently visible popup.

Used for cycling popup buffers with `ef-popup-cycle-forward' and
`ef-popup-cycle-backward'.")

(defvar ef-popup--buffer-state nil
  "The current state of the popup buffer.

When the value is nil it denotes the popup buffer is in a regular state.

When the value is 'promoted it denotes the popup buffer has been promoted
to a regular window state and will not be shown in the popup window.

When the value is 'demoted it denotes the popup buffer has been demoted
from a regular window state and will be shown in the popup window.")

(make-variable-buffer-local 'ef-popup--buffer-state)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commands
;;

;;;###autoload
(defun ef-popup-promote-buffer ()
  "Promotes the current buffer to a non-popup state."
  (interactive)
  (if-let* ((buffer (current-buffer))
            (_ (ef-popup--buffer-p buffer)))
      (progn
        (ef-popup--set-buffer-state buffer 'promoted)
        (setq ef-popup--buffer-list (remove buffer ef-popup--buffer-list))
        (delete-window (selected-window))
        (display-buffer buffer)
        ;; We already had a popup open, so make sure we select the previous
        ;; one and reopen it.
        (ef-popup-cycle-backward)
        (select-window (get-buffer-window buffer)))
    (user-error "Buffer is not a popup buffer: %s" (current-buffer))))

;;;###autoload
(defun ef-popup-demote-buffer ()
  "Demotes the current promoted popup buffer to a popup state."
  (interactive)
  (if-let* ((buffer (current-buffer))
            (_ (eq 'promoted (ef-popup--get-buffer-state buffer))))
      (progn
        (ef-popup--set-buffer-state buffer nil)
        (bury-buffer buffer)
        (setq ef-popup--buffer-list
              (ef-popup--move-to-front buffer ef-popup--buffer-list))
        (if (> (length (window-list)) 1)
            ;; We already have one or more open popups. Delete them first.
            (ef-popup--delete-all))
        (when-let* ((_ (= 1 (length (window-list))))
                    (win (car (window-list)))
                    (_ (ef-popup--buffer-p (window-buffer win)))
                    (file-buffer (cl-find-if-not #'ef-popup--buffer-p
                                                 (buffer-list))))
          ;; We only have one window repmaining and it's currently showing a
          ;; popup. In order to not show two popups at the same time, set
          ;; that window's buffer to the first non-popup buffer found.
          (set-window-buffer win file-buffer))
        (display-buffer buffer)
        (select-window (get-buffer-window buffer)))
    (user-error "Buffer is not a promoted popup buffer: %s" (current-buffer))))

;;;###autoload
(defun ef-popup-switch-buffer ()
  "Switch to popup buffer if `selected-window' is a popup window. Otherwise
switch to a non-popup buffer."
  (interactive)
  (if (ef-popup--buffer-p (window-buffer (selected-window)))
      (if (= (length (window-list)) 1)
          ;; The last window is showing a popup, which is made not dedicated,
          ;; therefore let the user freely select the buffer they want to
          ;; show.
          (call-interactively #'switch-to-buffer)
        (call-interactively #'ef-popup-switch-popup-buffer))
    (call-interactively #'ef-popup-switch-other-buffer)))

;;;###autoload
(defun ef-popup-switch-popup-buffer (buffer-name)
  "Switch to popup buffer BUFFER-NAME."
  (interactive
   (list
    (read-buffer "Switch to popup" (if (> (length ef-popup--buffer-list) 1)
                                       (cadr ef-popup--buffer-list)
                                     (car ef-popup--buffer-list))
                 (confirm-nonexistent-file-or-buffer)
                 #'ef-popup--buffer-p)))
  (when buffer-name
    (switch-to-buffer buffer-name)))

;;;###autoload
(defun ef-popup-switch-other-buffer (buffer-name)
  "Switch to non-popup buffer BUFFER-NAME."
  (interactive
   (list
    (read-buffer "Switch to buffer"
                 (if-let* ((bufs (seq-filter #'ef-popup--regular-buffer-p
                                             (buffer-list)))
                           (_ (> (length bufs) 1)))
                     (cadr bufs)
                   (car bufs))
                 (confirm-nonexistent-file-or-buffer)
                 #'ef-popup--regular-buffer-p)))
  (when buffer-name
    (switch-to-buffer buffer-name)))

;;;###autoload
(defun ef-popup-cycle-forward ()
  "Cycle visibility of popup windows forwards."
  (interactive)
  (if (= 0 (length (ef-popup--windows)))
      (ef-popup-toggle)
    (when ef-popup--buffer-list
      (setq ef-popup--buffer-list
            (cons (car (last ef-popup--buffer-list))
                  (butlast ef-popup--buffer-list)))
      (display-buffer (car ef-popup--buffer-list)))))

;;;###autoload
(defun ef-popup-cycle-backward ()
  "Cycle visibility of popup windows backwards."
  (interactive)
  (if (= 0 (length (ef-popup--windows)))
      (ef-popup-toggle)
    (when ef-popup--buffer-list
      (setq ef-popup--buffer-list
            (append (cdr ef-popup--buffer-list)
                    (list (car ef-popup--buffer-list))))
      (display-buffer (car ef-popup--buffer-list)))))

;;;###autoload
(defun ef-popup-toggle ()
  "Toggle visibility of the last opened popup window."
  (interactive)
  (if-let ((win (car (ef-popup--windows))))
      (delete-window win)
    (when-let ((buf (car (ef-popup--buffers))))
      (display-buffer buf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Private API
;;

(defun ef-popup--get-buffer-state (buf)
  "Returns the current popup buffer state for BUF.

See `ef-popup--buffer-state' for possible values. "
  (with-current-buffer buf
    ef-popup--buffer-state))

(defun ef-popup--set-buffer-state (buf state)
  "Sets the current popup buffer state of BUF to STATE.

See `ef-popup--buffer-state' for possible values."
  (with-current-buffer buf
    (setq ef-popup--buffer-state state)))

(defun ef-popup--move-to-front (elt list)
  "Add/mode ELT to the front of LIST."
  (cons elt (remove elt list)))

(defun ef-popup--delete-all ()
  "Delete all open popup windows."
  (dolist (win (ef-popup--windows))
    (if (> (length (window-list)) 1)
        (delete-window win))))

(defun ef-popup--buffer-match-rule-p (buf rule)
  "Return RULE if BUF matches RULE, `nil' otherwise."
  (cl-destructuring-bind (rule-name . rule-plist) rule
    (when-let ((found (shackle--match buf rule-name rule-plist)))
      (and (not (plist-get found :float))
           (not (eq 'promoted (ef-popup--get-buffer-state buf)))))))

(defun ef-popup--buffer-p (buf)
  "Return the matching rule from `shackle-rules' if BUF is a popup buffer,
`nil' otherwise."
  (when (consp buf)
    (setq buf (cdr buf)))
  (cl-find-if (apply-partially #'ef-popup--buffer-match-rule-p buf)
              shackle-rules))

(defun ef-popup--regular-buffer-p (buf)
  "Return t if BUF is a regular, non-popup buffer. Otherwise return nil."
  (when (consp buf)
    (setq buf (cdr buf)))
  (and (not (ef-popup--buffer-p buf))
       (not (minibufferp buf))))

(defun ef-popup--window-p (win)
  "Return the matching rule from `shackle-rules' if WIN is a popup window,
`nil' otherwise."
  (ef-popup--buffer-p (window-buffer win)))

(defun ef-popup--windows ()
  "Returns a list of open popup windows."
  (seq-filter #'ef-popup--window-p (window-list)))

(defun ef-popup--buffers ()
  "Returns a list of open popup buffers."
  (seq-filter #'ef-popup--buffer-p (buffer-list)))

(defun ef-popup--find-window (buf)
  "Find open popup window for BUF and return it. If no window was found,
return nil."
  (cl-find-if #'(lambda (v)
                  (eq (window-buffer v)
                      buf))
              (ef-popup--windows)))

(defun ef-popup--try-kill-ephemeral-popup-p (buf)
  "If BUF is an ephemeral popup buffer, kill it, otherwise do nothing.

Return the return value of `kill-buffer' if the conditions were satisfied,
nil otherwise."
  (when-let* ((rule (ef-popup--buffer-p buf))
              (_ (plist-get (cdr rule) :ephemeral)))
    (kill-buffer)))

(defun ef-popup--update-buffer-list ()
  "Function called from `window-configuration-change-hook' to update
`ef-popup--buffer-list' with any changes."
  (dolist (buf (cl-set-difference (ef-popup--buffers) ef-popup--buffer-list))
    (setq ef-popup--buffer-list
          (ef-popup--move-to-front buf ef-popup--buffer-list))))

(defun ef-popup--kill-buffer-hook ()
  "If an open popup window containing the buffer exists, check if more than
one pop up window is in the list. If there is, cycle to it, otherwise delete
the popup window. If the popup window was deleted, also remove it from
`ef-popup--buffer-list'."
  (let ((buf (current-buffer)))
    (when-let ((win (ef-popup--find-window buf)))
      (if (> (length ef-popup--buffer-list) 1)
          (ef-popup-cycle-backward)
        (if (> (length (window-list)) 1)
            (delete-window win))))

    (setq ef-popup--buffer-list
          (remove buf ef-popup--buffer-list))))

(defun ef-popup--shackle-display-buffer-action (orig-fun buffer alist)
  "If the newly opened window is a popup window, check if we already
have an open popup. If we do, call `delete-window' on the popup window
before opening a new one. Then mark the window as dedicated.

If BUFFER is not a popup buffer and `selected-window' is showing a popup,
select the buffer window with `select-window' if the buffer is already shown,
otherwise display the buffer using `display-buffer-use-some-window'."
  (if (ef-popup--buffer-p buffer)
      (progn
        (if (> (length (window-list)) 1)
            ;; We already have one or more open popups. Delete them first.
            (ef-popup--delete-all))
        (set-window-dedicated-p (apply orig-fun (list buffer alist)) t)
        ;;  Ensure the newly displayed buffer is at the front of
        ;; ef-popup--buffer-list.
        (unless (eq buffer (car ef-popup--buffer-list))
          (setq ef-popup--buffer-list
                (ef-popup--move-to-front buffer ef-popup--buffer-list))))
    (if (ef-popup--buffer-p (window-buffer (selected-window)))
        (if-let ((win (get-buffer-window buffer)))
            ;; Edge case: Sometimes will double a window if it's already
            ;; selected, eg. when triggered from `undo-tree-visualizer-mode'.
            (unless (eq (window-buffer win) buffer)
              (select-window win))
          (ef-popup--display-buffer-other-window buffer))
      ;; If it's a promoted popup, don't show it using the defined shackle
      ;; rules and display it like a regular buffer instead.
      (if (eq 'promoted (ef-popup--get-buffer-state buffer))
          (ef-popup--display-buffer-other-window buffer)
        (apply orig-fun (list buffer alist))))))

(defun ef-popup--display-buffer-other-window (buffer)
  "Display BUFFER in a non-popup buffer window."
  (display-buffer-use-some-window buffer
                                  '(nil (inhibit-same-window . t)
                                        (direction . above))))

(defun ef-popup--try-kill-ephemeral-popup-ad (orig-fun &res args)
  "Inhitbit ORIG-FUN in non-ephemeral popup buffers."
  (unless (ef-popup--try-kill-ephemeral-popup-p (current-buffer))
    (apply orig-fun args)))

(defun ef-popup--delete-window (orig-fun &rest args)
  "Prevent the last remaining window from staying dedicated, which would
prevent us from switching to other buffers."
  (apply orig-fun args)
  (if-let* ((wins (window-list))
            (_ (= (length wins) 1)))
      (set-window-dedicated-p (car wins) nil)))

(defun ef-popup--protect-popup-ad (orig-fun &rest args)
  (if (ef-popup--window-p (selected-window))
      (user-error "Current window is a protected popup window.")
    (apply orig-fun args)))

(defun ef-popup--without-open-popup-ad (orig-fun &rest args)
  (if (ef-popup--windows)
      (progn
        (ef-popup-toggle)
        (let ((res (apply orig-fun args)))
          (ef-popup-toggle)
          res))
    (apply orig-fun args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Modes
;;

;;;###autoload
(define-minor-mode ef-popup-mode
  "Toggle `ef-popup-mode'.
This minor mode enables pedantic popup management for shackle
popups. "
  :global t
  :keymap (let ((map (make-sparse-keymap))) map)
  :lighter ""
  :group 'ef-popup
  (if ef-popup-mode
      (progn
        (add-hook 'window-configuration-change-hook
                  'ef-popup--update-buffer-list)
        (add-hook 'kill-buffer-hook 'ef-popup--kill-buffer-hook)
        (advice-add 'delete-window :around #'ef-popup--delete-window)
        (advice-add 'quit-window
                    :around #'ef-popup--try-kill-ephemeral-popup-ad)
        (advice-add 'quit-restore-window
                    :around #'ef-popup--try-kill-ephemeral-popup-ad)
        (advice-add 'shackle-display-buffer-action
                    :around #'ef-popup--shackle-display-buffer-action)
        (if (fboundp 'evil-window-rotate-downwards)
            (advice-add 'evil-window-rotate-downwards
                        :around #'ef-popup--without-open-popup-ad))
        (if (fboundp 'evil-window-rotate-upwards)
            (advice-add 'evil-window-rotate-upwards
                        :around #'ef-popup--without-open-popup-ad))
        (advice-add 'split-window :around #'ef-popup--protect-popup-ad)
        (ef-popup--update-buffer-list))
    (remove-hook 'window-configuration-change-hook
                 'ef-popup--update-buffer-list)
    (remove-hook 'kill-buffer-hook 'ef-popup--kill-buffer-hook)
    (advice-remove 'delete-window #'ef-popup--delete-window)
    (advice-remove 'quit-window #'ef-popup--try-kill-ephemeral-popup-ad)
    (advice-remove 'quit-restore-window
                   #'ef-popup--try-kill-ephemeral-popup-ad)
    (advice-remove 'shackle-display-buffer-action
                   #'ef-popup--shackle-display-buffer-action)
    (if (fboundp 'evil-window-rotate-downwards)
        (advice-remove 'evil-window-rotate-downwards
                       #'ef-popup--without-open-popup-ad))
    (if (fboundp 'evil-window-rotate-upwards)
        (advice-remove 'evil-window-rotate-upwards
                       #'ef-popup--without-open-popup-ad))
    (advice-remove 'split-window #'ef-popup--protect-popup-ad)
    (setq ef-popup--buffer-list nil)
    (dolist (buf (buffer-list))
      (ef-popup--set-buffer-state buf nil))
    (ef-popup--delete-all)))

(provide 'ef-popup)
