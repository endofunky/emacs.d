;;; poe.el --- A popup manager -*- lexical-binding: t; -*-
(require 'cl-extra)

(defgroup poe nil
  "A popup manager."
  :group 'convenience)

(defvar poe-rules nil)

(defmacro poe-rule (key &rest plist)
  (declare (indent 0))
  `(push '(,key ,@plist) poe-rules))

(defun poe-match (buffer-or-name)
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

(defun poe--display-buffer-condition (buffer _action)
  (poe-match buffer))

(defun poe--display-buffer-action (buffer alist)
  (poe--display-buffer buffer alist (poe-match buffer)))

(defun poe--display-buffer (buffer alist rule)
  (message "Display %s with alist: %s rule: %s" buffer alist rule))

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
;; Modes
;; ----------------------------------------------------------------------------

(defvar poe-popup-mode-map (make-sparse-keymap)
  "Active keymap in `poe-mode' popup windows. See `poe-popup-mode'.")

(define-minor-mode poe-popup-mode
  "Minor mode for individual `poe-mode' buffers to be shown in popup windows."
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
