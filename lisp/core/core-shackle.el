;;; core-shackle.el --- Popup management -*- lexical-binding: t; -*-
(require 'core-lib)
(require 'cl-extra)
(require 'cl-macs)
(require 'cl-seq)
(require 'subr-x)

(defvar ef-popup--buffer-list '()
  "List of popup buffers in the order they were opened in.

The `car' of this list will be the most recently visible popup.

Used for cycling popup buffers with `+popup-cycle-forward' and
`+popup-cycle-backward'.")

(defconst ef-popup-defaults '(:align below :size .25 :popup t :select t)
  "Default values for `shackle-rules' applied to popup buffers created
with `+add-popup'.")

(defvar ef-popup--buffer-state nil
  "The current state of the popup buffer.

When the value is nil it denotes the popup buffer is in a regular state.

When the value is promoted it denotes the popup buffer has been promoted
to a regular window state and will not be shown in the popup window.")

(make-variable-buffer-local 'ef-popup--buffer-state)

(use-package shackle
  :demand t
  :custom
  (shackle-select-reused-windows nil)
  (shackle-default-alignment 'below)
  (shackle-default-size 0.3)
  (shackle-default-rule '(:same t))
  :commands (shackle-mode)
  :functions (+shackle
              +add-popup)
  :config
  (general-define-key
   :states '(normal insert visual motion replace)
   :keymaps 'override
   "M-h" '+popup-cycle-backward
   "M-j" '+popup-demote-buffer
   "M-k" '+popup-promote-buffer
   "M-l" '+popup-cycle-forward
   "M-P" '+popup-switch-popup-buffer
   "M-p" '+popup-toggle)

  (defun +shackle (shackle &rest shackles)
    "Adds one or more shackle rules to `shackle-rules'"
    (dolist (rule (cons shackle shackles))
      (add-to-list 'shackle-rules rule)))

  (defun +add-popup (mode &rest rules)
    (add-to-list 'shackle-rules
                 (cons mode (+plist-merge ef-popup-defaults rules))))

  (+shackle '(Info-mode :align right :size .5 :popup nil
                          :select t :float t :inhibit-window-quit t))

  (+add-popup " *Metahelp*" :ephemeral t)
  (+add-popup " *undo-tree*" :ephemeral t)
  (+add-popup "*Apropos*" :size .3 :ephemeral t)
  (+add-popup "*Backtrace*")
  (+add-popup "*Checkdoc Status*" :ephemeral t)
  (+add-popup "*Compile-Log*" :ephemeral t)
  (+add-popup "*Command History*")
  (+add-popup "*Help*" :ephemeral t)
  (+add-popup "*Messages*")
  (+add-popup "*Occur*" :ephemeral t)
  (+add-popup "*Pp Eval Output*")
  (+add-popup "*Warnings*" :ephemeral t)
  (+add-popup "*compilation*")
  (+add-popup "\\`\\*WoMan.*?\\*\\'" :regexp t :ephemeral t)
  (+add-popup 'calendar-mode :ephemeral t)
  (+add-popup 'comint-mode)
  (+add-popup 'compilation-mode)

  (shackle-mode t))

(declare-function shackle--match "shackle")
(defvar shackle-rules)

(defun +popup--get-buffer-state (buf)
  "Returns the current popup buffer state for BUF.

See `ef-popup--buffer-state' for possible values. "
  (with-current-buffer buf
    ef-popup--buffer-state))

(defun +popup--set-buffer-state (buf state)
  "Sets the current popup buffer state of BUF to STATE.

See `ef-popup--buffer-state' for possible values."
  (with-current-buffer buf
    (setq ef-popup--buffer-state state)))

(defun +popup--delete-all ()
  "Delete all open popup windows."
  (dolist (win (+popup--windows))
    (if (> (length (window-list)) 1)
        (delete-window win))))

(defun +popup-promote-buffer ()
  "Promotes the current buffer to a non-popup state."
  (interactive)
  (if-let* ((buffer (current-buffer))
            (ok (+popup--buffer-p buffer)))
      (progn
        (+popup--set-buffer-state buffer 'promoted)
        (setq ef-popup--buffer-list (remove buffer ef-popup--buffer-list))
        (delete-window (selected-window))
        (display-buffer buffer)
        ;; We already had a popup open, so make sure we select the previous one
        ;; and reopen it.
        (+popup-cycle-backward)
        (select-window (get-buffer-window buffer)))
    (user-error "Buffer is not a popup buffer: %s" (current-buffer))))

(defun +popup-demote-buffer ()
  "Demotes the current promoted popup buffer to a popup state."
  (interactive)
  (if-let* ((buffer (current-buffer))
            (ok (eq 'promoted (+popup--get-buffer-state buffer))))
      (progn
        (+popup--set-buffer-state buffer nil)
        (bury-buffer buffer)
        (setq ef-popup--buffer-list
              (+move-to-front buffer ef-popup--buffer-list))
        (if (> (length (window-list)) 1)
            ;; We already have one or more open popups. Delete them first.
            (+popup--delete-all))
        (when-let* ((ok (= 1 (length (window-list))))
                    (win (car (window-list)))
                    (ok (+popup--buffer-p (window-buffer win)))
                    (file-buffer (cl-find-if-not #'+popup--buffer-p (buffer-list))))
          ;; We only have one window repmaining and it's currently showing a
          ;; popup. In order to not show two popups at the same time, set
          ;; that window's buffer to the first non-popup buffer found.
          (set-window-buffer win file-buffer))
        (display-buffer buffer)
        (select-window (get-buffer-window buffer)))
    (user-error "Buffer is not a promoted popup buffer: %s" (current-buffer))))

(defun +popup--buffer-match-rule-p (buf rule)
  "Return RULE if BUF matches RULE, `nil' otherwise."
  (cl-destructuring-bind (rule-name . rule-plist) rule
    (when-let ((found (shackle--match buf rule-name rule-plist)))
      (and (not (plist-get found :float))
           (not (eq 'promoted (+popup--get-buffer-state buf)))))))

(defun +popup--buffer-p (buf)
  "Return the matching rule from `shackle-rules' if BUF is a popup buffer,
`nil' otherwise."
  (when (consp buf)
    (setq buf (cdr buf)))
  (cl-find-if (apply-partially #'+popup--buffer-match-rule-p buf)
              shackle-rules))

(defun +popup--regular-buffer-p (buf)
  "Return t if BUF is a regular, non-popup buffer. Otherwise return nil."
  (when (consp buf)
    (setq buf (cdr buf)))
  (and (not (+popup--buffer-p buf))
       (not (minibufferp buf))))

(defun +popup--window-p (win)
  "Return the matching rule from `shackle-rules' if WIN is a popup window,
`nil' otherwise."
  (+popup--buffer-p (window-buffer win)))

(defun +popup--windows ()
  "Returns a list of open popup windows."
  (seq-filter #'+popup--window-p (window-list)))

(defun +popup--buffers ()
  "Returns a list of open popup buffers."
  (seq-filter #'+popup--buffer-p (buffer-list)))

(defun +popup-switch-buffer ()
  "Switch to popup buffer if `selected-window' is a popup window. Otherwise
switch to a non-popup buffer."
  (interactive)
  (if (+popup--buffer-p (window-buffer (selected-window)))
      (if (= (length (window-list)) 1)
          ;; The last window is showing a popup, which is made not dedicated,
          ;; therefore let the user freely select the buffer they want to
          ;; show.
          (call-interactively #'switch-to-buffer)
        (call-interactively #'+popup-switch-popup-buffer))
    (call-interactively #'+popup-switch-other-buffer)))

(defun +popup-switch-popup-buffer (buffer-name)
  "Switch to popup buffer BUFFER-NAME."
  (interactive
   (list
    (read-buffer "Switch to popup: " (if (> (length ef-popup--buffer-list) 1)
                                         (cadr ef-popup--buffer-list)
                                       (car ef-popup--buffer-list))
                 (confirm-nonexistent-file-or-buffer)
                 #'+popup--buffer-p)))
  (when buffer-name
    (switch-to-buffer buffer-name)))

(defun +popup-switch-other-buffer (buffer-name)
  "Switch to non-popup buffer BUFFER-NAME."
  (interactive
   (list
    (read-buffer "Switch to buffer: "
                 (if-let* ((bufs (seq-filter #'+popup--regular-buffer-p
                                             (buffer-list)))
                           (ok (> (length bufs) 1)))
                     (cadr bufs)
                   (car bufs))
                 (confirm-nonexistent-file-or-buffer)
                 #'+popup--regular-buffer-p)))
  (when buffer-name
    (switch-to-buffer buffer-name)))

(defun +popup-cycle-forward ()
  "Cycle visibility of popup windows forwards."
  (interactive)
  (if (= 0 (length (+popup--windows)))
      (+popup-toggle)
    (when ef-popup--buffer-list
      (setq ef-popup--buffer-list
            (cons (car (last ef-popup--buffer-list))
                  (butlast ef-popup--buffer-list)))
      (display-buffer (car ef-popup--buffer-list)))))

(defun +popup-cycle-backward ()
  "Cycle visibility of popup windows backwards."
  (interactive)
  (if (= 0 (length (+popup--windows)))
      (+popup-toggle)
    (when ef-popup--buffer-list
      (setq ef-popup--buffer-list
            (append (cdr ef-popup--buffer-list)
                    (list (car ef-popup--buffer-list))))
      (display-buffer (car ef-popup--buffer-list)))))

(defun +popup-toggle ()
  "Toggle visibility of the last opened popup window."
  (interactive)
  (if-let ((win (car (+popup--windows))))
      (delete-window win)
    (when-let ((buf (car ef-popup--buffer-list)))
      (display-buffer buf))))

(defun +popup--find-window (buf)
  "Find open popup window for BUF and return it. If no window was found,
return nil."
  (cl-find-if #'(lambda (v)
                  (eq (window-buffer v)
                      buf))
              (+popup--windows)))

(defun +popup--try-kill-ephemeral-popup-p (buf)
  "If BUF is an ephemeral popup buffer, kill it, otherwise do nothing.

Return the return value of `kill-buffer' if the conditions were satisfied,
nil otherwise."
  (when-let* ((rule (+popup--buffer-p buf))
              (ok (plist-get (cdr rule) :ephemeral)))
    (kill-buffer)))

(defun +popup--update-buffer-list ()
  "Function called from `window-configuration-change-hook' to update
`ef-popup--buffer-list' with any changes."
  (dolist (buf (cl-set-difference (+popup--buffers) ef-popup--buffer-list))
    (setq ef-popup--buffer-list (+move-to-front buf ef-popup--buffer-list))))

(add-hook 'window-configuration-change-hook #'+popup--update-buffer-list)

(defun +popup--kill-buffer-hook ()
  "If an open popup window containing the buffer exists, check if more than
one pop up window is in the list. If there is, cycle to it, otherwise delete
the popup window. If the popup window was deleted, also remove it from
`ef-popup--buffer-list'."
  (let ((buf (current-buffer)))
    (when-let ((win (+popup--find-window buf)))
      (if (> (length ef-popup--buffer-list) 1)
          (+popup-cycle-backward)
        (if (> (length (window-list)) 1)
            (delete-window win))))

    (setq ef-popup--buffer-list
          (remove buf ef-popup--buffer-list))))

(add-hook 'kill-buffer-hook #'+popup--kill-buffer-hook)

(defadvice shackle-display-buffer-action (around ef-single-popup activate)
  "If the newly opened window is a popup window, check if we already
have an open popup. If we do, call `delete-window' on the popup window
before opening a new one. Then mark the window as dedicated.

If BUFFER is not a popup buffer and `selected-window' is showing a popup,
select the buffer window with `select-window' if the buffer is already shown,
otherwise display the buffer using `display-buffer-use-some-window'."
  (if (+popup--buffer-p buffer)
      (progn
        (when (> (length (window-list)) 1)
          ;; We already have one or more open popups. Delete them first.
          (+popup--delete-all))
        (set-window-dedicated-p ad-do-it t)
        ;;  straight the newly displayed buffer is at the front of
        ;; ef-popup--buffer-list.
        (unless (eq buffer (car ef-popup--buffer-list))
          (setq ef-popup--buffer-list
                (+move-to-front buffer ef-popup--buffer-list))))
    (if (+popup--buffer-p (window-buffer (selected-window)))
        (if-let ((win (get-buffer-window buffer)))
            ;; Edge case: Sometimes will double a window if it's already
            ;; selected, eg. when triggered from `undo-tree-visualizer-mode'.
            (unless (eq (window-buffer win) buffer)
              (select-window win))
          (+popup-display-buffer-other-window buffer))
      ;; If it's a promoted popup, don't show it using the defined shackle
      ;; rules and display it like a regular buffer instead.
      (if (eq 'promoted (+popup--get-buffer-state buffer))
          (+popup-display-buffer-other-window buffer))
      ad-do-it)))

(defun +popup-display-buffer-other-window (buffer)
  "Display BUFFER in a non-popup buffer window."
  (display-buffer-use-some-window buffer
                                  '(nil (inhibit-same-window . t)
                                        (direction . above))))

(defadvice quit-window (around ef-popup-quit-window activate)
  "Inhitbit `quit-window' in non-ephemeral popup buffers."
  (unless (+popup--try-kill-ephemeral-popup-p (current-buffer))
    ad-do-it))

(defadvice quit-restore-window (around ef-popup-quit-restore-window activate)
  "Inhitbit `quit-restore-window' in non-ephemeral popup buffers."
  (unless (+popup--try-kill-ephemeral-popup-p (current-buffer))
    ad-do-it))

(defadvice delete-window (around ef-popup--delete-window activate)
  "Prevent the last remaining window from staying dedicated, which would prevent
us from switching to other buffers."
  ad-do-it
  (if-let* ((wins (window-list))
            (_ (= (length wins) 1)))
      (set-window-dedicated-p (car wins) nil)))

(defadvice split-window (around ef-popup-split-window activate)
  "Prevent popup windows being split."
  (if (+popup--window-p (selected-window))
      (user-error "Cannot split windows in a popup window")
    ad-do-it))

(defun +without-open-popup-a (orig-fun &rest args)
  (if (+popup--windows)
      (progn
        (+popup-toggle)
        (let ((res (apply orig-fun args)))
          (+popup-toggle)
          res))
    (apply orig-fun args)))

(advice-add 'evil-window-rotate-downwards :around #'+without-open-popup-a)
(advice-add 'evil-window-rotate-upwards :around #'+without-open-popup-a)

(provide 'core-shackle)
