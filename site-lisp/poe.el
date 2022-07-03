;;; poe.el --- A popup manager -*- lexical-binding: t; -*-
(defgroup poe nil
  "A popup manager."
  :group 'convenience)

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
