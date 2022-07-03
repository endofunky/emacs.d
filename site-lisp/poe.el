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
  (let* ((buffer (get-buffer buffer-or-name))
         (buffer-major-mode (buffer-local-value 'major-mode buffer))
         (buffer-name (buffer-name buffer)))
    (cl-loop
     for (condition . plist) in poe-rules
     when (or (and (symbolp condition)
                   (eq condition
                       buffer-major-mode))
              (and (stringp condition)
                   (or (string= condition buffer-name)
                       (and (plist-get plist :regexp)
                            (string-match condition
                                          buffer-name)))))
     return plist)))

;; ----------------------------------------------------------------------------
;; Modes
;; ----------------------------------------------------------------------------

(defvar poe-buffer-mode-map (make-sparse-keymap)
  "Active keymap in `poe-mode' popup windows. See `poe-buffer-mode'.")

(define-minor-mode poe-buffer-mode
  "Minor mode for individual `poe-mode' buffers to be shown in popup windows."
  :group 'poe
  :lighter nil
  :keymap poe-buffer-mode-map)

(defvar poe-mode-map (make-sparse-keymap)
  "Global keymap for `poe-mode'.")

(define-minor-mode poe-mode
  "Toggle `poe' on or off."
  :group 'poe
  :global t
  :lighter nil
  :keymap poe-mode-map)

(provide 'poe)
