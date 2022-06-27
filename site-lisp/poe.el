(require 'cl-seq)

(defgroup poe nil
  "A popup manager."
  :group 'convenience)

(defvar poe--buffers '()
  "List of popup buffers in the order they were opened in.

The `car' of this list will be the most recently visible popup.

Used for cycling popup buffers with `ef-popup-cycle-forward' and
`ef-popup-cycle-backward'.")

(defun poe--popup-p (buf)
  nil)

(defun poe--move-to-front (elt list)
  "Add/mode ELT to the front of LIST."
  (cons elt (remove elt list)))

(defun ef-popup--buffers ()
  "Returns a list of open popup buffers."
  (seq-filter #'poe--popup-p (buffer-list)))

(defun poe--update-buffers ()
  "Function called from `window-configuration-change-hook' to update
`poe--buffers' with any changes."
  (dolist (buf (cl-set-difference (ef-popup--buffers) poe--buffers))
    (setq poe--buffers (poe--move-to-front buf poe--buffers))))

;; ----------------------------------------------------------------------------
;; Commands
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;; Modes
;; ----------------------------------------------------------------------------

(defvar poe-buffer-mode-map (make-sparse-keymap)
  "Active keymap in `poe-mode' popup windows. See `poe-buffer-mode'.")

(define-minor-mode poe-buffer-mode
  "Minor mode for individual `poe-mode' popup windows."
  :group 'poe
  :lighter nil
  :keymap poe-buffer-mode-map)

(defvar poe-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map "M-p" #'poe-toggle)
    ;; (define-key map "M-h" #'poe-previous)
    ;; (define-key map "M-l" #'poe-next)
    map)
  "Global keymap for `poe-mode'.")

(define-minor-mode poe-mode
  "Toggle `poe' on or off."
  :group 'poe
  :global t
  :lighter nil
  :keymap poe-mode-map
  (if poe-mode
      (add-hook 'window-configuration-change-hook #'poe--update-buffers)
    (remove-hook 'window-configuration-change-hook #'poe--update-buffers)))

(provide 'poe)
