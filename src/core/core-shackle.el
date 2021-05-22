(require 'core-lib)
(require 'cl-extra)
(require 'cl-macs)
(require 'cl-seq)
(require 'subr-x)

(defvar ef-popup-buffer-list '()
  "List of popup buffers in the order they were opened in.

The `car' of this list will be the most recently visible popup.

Used for cycling popup buffers with `ef-popup-cycle-forward' and
`ef-popup-cycle-backward'.")

(defconst ef-popup-defaults '(:align below :size .4 :popup t :select t)
  "Default values for `shackle-rules' applied to popup buffers created
with `ef-add-popup'.")

(defvar ef-popup-buffer-state nil
  "The current state of the popup buffer.

When the value is nil it denotes the popup buffer is in a regular state.

When the value is 'promoted it denotes the popup buffer has been promoted
to a regular window state and will not be shown in the popup window.")

(make-variable-buffer-local 'ef-popup-buffer-state)

(use-package shackle
  :ensure t
  :custom
  (shackle-select-reused-windows nil)
  (shackle-default-alignment 'below)
  (shackle-default-size 0.4)
  (shackle-default-rule '(:same t))
  :functions (ef-shackle
              ef-add-popup)
  :config
  (defun ef-shackle (shackle &rest shackles)
    "Adds one or more shackle rules to `shackle-rules'"
    (dolist (rule (cons shackle shackles))
      (add-to-list 'shackle-rules rule)))

  (defun ef-add-popup (mode &rest rules)
    (add-to-list 'shackle-rules
                 (cons mode (ef-plist-merge ef-popup-defaults rules))))

  (ef-add-popup " *Metahelp*" :ephemeral t)
  (ef-add-popup " *undo-tree*" :ephemeral t)
  (ef-add-popup "*Apropos*" :size .3 :ephemeral t)
  (ef-add-popup "*Backtrace*")
  (ef-add-popup "*Checkdoc Status*" :ephemeral t)
  (ef-add-popup "*Command History*")
  (ef-add-popup "*Help*" :ephemeral t)
  (ef-add-popup "*Messages*")
  (ef-add-popup "*Occur*" :ephemeral t)
  (ef-add-popup "*Pp Eval Output*")
  (ef-add-popup "*Warnings*" :ephemeral t)
  (ef-add-popup "*company-documentation*" :ephemeral t)
  (ef-add-popup "*compilation*")
  (ef-add-popup "\\`\\*WoMan.*?\\*\\'" :regexp t :ephemeral t)
  (ef-add-popup "\\`\\*info.*?\\*\\'" :regexp t :ephemeral t :size 0.5)
  (ef-add-popup 'Info-mode :ephemeral t :size 0.5)
  (ef-add-popup 'comint-mode)
  (ef-add-popup 'compilation-mode)
  (ef-add-popup 'info-mode :ephemeral t :size 0.5)

  (shackle-mode t))

(declare-function shackle--match "shackle")

(defun ef-popup-get-buffer-state (buf)
  "Returns the current popup buffer state for BUF.

See `ef-popup-buffer-state' for possible values. "
  (with-current-buffer buf
    ef-popup-buffer-state))

(defun ef-popup-set-buffer-state (buf state)
  "Sets the current popup buffer state of BUF to STATE.

See `ef-popup-buffer-state' for possible values."
  (with-current-buffer buf
    (setq ef-popup-buffer-state state)))

(defun ef-popup-promote-buffer ()
  "Promotes the current buffer to a non-popup state."
  (interactive)
  (if-let* ((buffer (current-buffer))
            (_ (ef-popup-buffer-p buffer)))
      (progn
        (ef-popup-set-buffer-state buffer 'promoted)
        (setq ef-popup-buffer-list (remove buffer ef-popup-buffer-list))
        (delete-window (selected-window))
        (display-buffer buffer)
        (ef-popup-cycle-backward)
        (select-window (get-buffer-window buffer)))
    (message "Buffer is not a popup buffer: %s" (current-buffer))))

(defun ef-popup-demote-buffer ()
  "Demotes the current promoted popup buffer to a popup state."
  (interactive)
  (if-let* ((buffer (current-buffer))
            (_ (eq 'promoted (ef-popup-get-buffer-state buffer))))
      (progn
        (ef-popup-set-buffer-state buffer nil)
        (bury-buffer buffer)
        (setq ef-popup-buffer-list
              (ef-move-to-front buffer ef-popup-buffer-list))
        (when-let ((_ (> (length (window-list)) 1))
                   (open-popups (ef-popup-windows)))
          ;; We already have an open popup. Delete it first.
          (delete-window (car open-popups)))
        (display-buffer buffer)
        (select-window (get-buffer-window buffer)))
    (message "Buffer is not a promoted popup buffer: %s" (current-buffer))))

(defun ef-popup-buffer-match-rule-p (buf rule)
  "Return RULE if BUF matches RULE, `nil' otherwise."
  (cl-destructuring-bind (rule-name . rule-plist) rule
    (when-let ((found (shackle--match buf rule-name rule-plist)))
      (and (not (plist-get found :float))
           (not (eq 'promoted (ef-popup-get-buffer-state buf)))))))

(defun ef-popup-buffer-p (buf)
  "Return the matching rule from `shackle-rules' if BUF is a popup buffer,
`nil' otherwise."
  (cl-find-if (apply-partially #'ef-popup-buffer-match-rule-p buf)
              shackle-rules))

(defun ef-popup-window-p (win)
  "Return the matching rule from `shackle-rules' if WIN is a popup window,
`nil' otherwise."
  (ef-popup-buffer-p (window-buffer win)))

(defun ef-popup-windows ()
  "Returns a list of open popup windows."
  (seq-filter #'ef-popup-window-p (window-list)))

(defun ef-popup-buffers ()
  "Returns a list of open popup buffers."
  (seq-filter #'ef-popup-buffer-p (buffer-list)))

(defun ef-popup-cycle-forward ()
  "Cycle visibility of popup windows forwards."
  (interactive)
  (if (= 0 (length (ef-popup-windows)))
      (ef-popup-toggle))
  (when ef-popup-buffer-list
    (setq ef-popup-buffer-list
          (cons (car (last ef-popup-buffer-list))
                (butlast ef-popup-buffer-list)))
    (display-buffer (car ef-popup-buffer-list))))

(defun ef-popup-cycle-backward ()
  "Cycle visibility of popup windows backwards."
  (interactive)
  (if (= 0 (length (ef-popup-windows)))
      (ef-popup-toggle)
    (when ef-popup-buffer-list
      (setq ef-popup-buffer-list
            (append (cdr ef-popup-buffer-list)
                    (list (car ef-popup-buffer-list))))
      (display-buffer (car ef-popup-buffer-list)))))

(defun ef-popup-toggle ()
  "Toggle visibility of the last opened popup window."
  (interactive)
  (if-let ((win (car (ef-popup-windows))))
      (delete-window win)
    (when-let ((buf (car (ef-popup-buffers))))
      (display-buffer buf))))

(defun ef-popup-find-window (buf)
  "Find open popup window for BUF and return it. If no window was found,
return nil."
  (cl-find-if #'(lambda (v)
                  (eq (window-buffer v)
                      buf))
              (ef-popup-windows)))

(defun ef-popup-try-kill-ephemeral-popup-p (buf)
  "If BUF is an ephemeral popup buffer, kill it, otherwise do nothing.

Return the return value of `kill-buffer' if the conditions were satisfied,
nil otherwise."
  (when-let* ((rule (ef-popup-buffer-p buf))
              (_ (plist-get (cdr rule) :ephemeral)))
    (kill-buffer)))

(defun ef-popup-update-buffer-list ()
  "Function called from `window-configuration-change-hook' to update
`ef-popup-buffer-list' with any changes."
  (dolist (buf (cl-set-difference (ef-popup-buffers) ef-popup-buffer-list))
    (setq ef-popup-buffer-list (ef-move-to-front buf ef-popup-buffer-list))))

(add-hook 'window-configuration-change-hook #'ef-popup-update-buffer-list)

(defun ef-popup-killed-buffer-hook ()
  "If an open popup window containing the buffer exists, check if more than
one pop up window is in the list. If there is, cycle to it, otherwise delete
the popup window. If the popup window was deleted, also remove it from
`ef-popup-buffer-list'."
  (let ((buf (current-buffer)))
    (when-let ((win (ef-popup-find-window buf)))
      (if (> (length ef-popup-buffer-list) 1)
          (ef-popup-cycle-backward)
        (if (> (length (window-list)) 1)
            (delete-window win))))

    (setq ef-popup-buffer-list
          (remove buf ef-popup-buffer-list))))

(add-hook 'kill-buffer-hook #'ef-popup-killed-buffer-hook)

(defadvice shackle-display-buffer-action (around ef-single-popup activate)
  "If the newly opened window is a popup window, check if we already
have an open popup. If we do, call `delete-window' on the popup window
before opening a new one. Then mark the window as dedicated.

If BUFFER is not a popup buffer and `selected-window' is showing a popup,
select the buffer window with `select-window' if the buffer is already shown,
otherwise display the buffer using `display-buffer-use-some-window'."
  (if (ef-popup-buffer-p buffer)
      (progn
        (when-let ((_ (> (length (window-list)) 1))
                   (open-popups (ef-popup-windows)))
          ;; We already have an open popup. Delete it first.
          (delete-window (car open-popups)))
        (set-window-dedicated-p ad-do-it t)
        ;;  Ensure the newly displayed buffer is at the front of
        ;; ef-popup-buffer-list.
        (unless (eq buffer (car ef-popup-buffer-list))
          (setq ef-popup-buffer-list
                (ef-move-to-front buffer ef-popup-buffer-list))))
    (if (ef-popup-buffer-p (window-buffer (selected-window)))
        (if-let ((win (get-buffer-window buffer)))
            ;; Edge case: Sometimes will double a window if it's already
            ;; selected, eg. when triggered from `undo-tree-visualizer-mode'.
            (unless (eq (window-buffer win) buffer)
              (select-window win))
          (ef-popup-display-buffer-other-window buffer))
      ;; If it's a promoted popup, don't show it using the defined shackle
      ;; rules and display it like a regular buffer instead.
      (if (eq 'promoted (ef-popup-get-buffer-state buffer))
          (ef-popup-display-buffer-other-window buffer)
        ad-do-it))))

(defun ef-popup-display-buffer-other-window (buffer)
  "Display BUFFER in a non-popup buffer window."
  (display-buffer-use-some-window buffer
                                  '(nil (inhibit-same-window . t)
                                        (direction . above))))

(defadvice quit-window (around ef-popup-quit-window activate)
  "Inhitbit `quit-window' in non-ephemeral popup buffers."
  (unless (ef-popup-try-kill-ephemeral-popup-p (current-buffer))
    ad-do-it))

(defadvice quit-restore-window (around ef-popup-quit-restore-window activate)
  "Inhitbit `quit-restore-window' in non-ephemeral popup buffers."
  (unless (ef-popup-try-kill-ephemeral-popup-p (current-buffer))
    ad-do-it))

(defadvice delete-window (around ef-popup-delete-window activate)
  "Prevent the last remaining window from staying dedicated, which would prevent
us from switching to other buffers."
  ad-do-it
  (if-let* ((wins (window-list))
            (_ (= (length wins) 1)))
      (set-window-dedicated-p (car wins) nil)))

(general-define-key
 :states '(normal insert visual motion replace)
 :keymaps 'override
 "M-h" 'ef-popup-cycle-backward
 "M-j" 'ef-popup-demote-buffer
 "M-k" 'ef-popup-promote-buffer
 "M-l" 'ef-popup-cycle-forward
 "M-p" 'ef-popup-toggle)

(provide 'core-shackle)
