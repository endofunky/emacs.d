(require 'core-lib)
(require 'cl-extra)
(require 'cl-macs)
(require 'cl-seq)
(require 'subr-x)

(defvar ef-popup-buffer-list '())
(defconst ef-popup-defaults
  '(:align below :size .4 :popup t :select t))

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

  (ef-add-popup " *undo-tree*")
  (ef-add-popup "*company-documentation*")
  (ef-add-popup "*compilation*")
  (ef-add-popup 'compilation-mode)
  (ef-add-popup "\\`\\*WoMan.*?\\*\\'" :regexp t)
  (ef-add-popup "*info*")
  (ef-add-popup "*Checkdoc Status*")
  (ef-add-popup "*Apropos*" :size .3)
  (ef-add-popup " *Metahelp*")
  (ef-add-popup "*Help*")

  (shackle-mode t))

(defmacro ef-define-repl (name buf fn)
  "Define a shackle REPL wrapper function.

Define a new function `NAME' for buffer `BUF' created by calling function `FN'
When `shackle-rules' is bound, will add a popup & select rule for the given
buffer."
  `(progn
     (when (boundp 'shackle-rules)
       (ef-add-popup ,buf))
     (defun ,name ()
       (interactive)
       (let ((buffer (get-buffer ,buf))
             (window (get-buffer-window ,buf)))
         (cond ((null buffer)
                (call-interactively ,fn)
                (when (fboundp 'evil-change-state)
                  (evil-change-state 'normal)))
               (window
                (if (one-window-p)
                    (switch-to-prev-buffer nil t)
                  (delete-window window)))
               (t
                (pop-to-buffer ,buf)
                (when (fboundp 'evil-change-state)
                  (evil-change-state 'normal))))))))

(defun ef-popup-buffer-match-rule-p (buf rule)
  (cl-destructuring-bind (rule-name . rule-plist) rule
    (and (plist-get rule-plist :popup)
         (not (plist-get rule-plist :popup-float))
         (or (and (plist-get :regexp rule-plist)
                  (string-match rule-name
                                (buffer-name buf)))
             (and (stringp rule-name)
                  (string= rule-name
                           (buffer-name buf)))
             (and (symbolp rule-name)
                  (eq rule-name (with-current-buffer buf
                                  major-mode)))))))

(defun ef-popup-buffer-p (buf)
  (cl-some (apply-partially #'ef-popup-buffer-match-rule-p buf)
           shackle-rules))

(defun ef-popup-window-p (win)
  (ef-popup-buffer-p (window-buffer win)))

(defun ef-popup-windows ()
  (seq-filter #'ef-popup-window-p (window-list)))

(defun ef-popup-buffers ()
  (seq-filter #'ef-popup-buffer-p (buffer-list)))

(defun ef-popup-cycle-forward ()
  "Cycle visibility of popup windows forwards."
  (interactive)
  (if (= 0 (length (ef-popup-windows)))
      (ef-popup-toggle)
    (if-let* ((curr (car (ef-popup-buffers)))
              (pos (cl-position curr ef-popup-buffer-list)))
        (if (= pos (- (length ef-popup-buffer-list) 1))
            (display-buffer (car ef-popup-buffer-list))
          (display-buffer (nth (+ pos 1) ef-popup-buffer-list))))))

(defun ef-popup-cycle-backward ()
  "Cycle visibility of popup windows backwards."
  (interactive)
  (if (= 0 (length (ef-popup-windows)))
      (ef-popup-toggle)
    (if-let* ((curr (car (ef-popup-buffers)))
              (pos (cl-position curr ef-popup-buffer-list)))
        (if (= pos 0)
            (display-buffer (car (last ef-popup-buffer-list)))
          (display-buffer (nth (- pos 1) ef-popup-buffer-list))))))

(defun ef-popup-toggle ()
  "Toggle visibility of the last opened popup window."
  (interactive)
  (if-let ((win (car (ef-popup-windows))))
      (delete-window win)
    (when-let ((buf (car (ef-popup-buffers))))
      (display-buffer buf))))

(defun ef-popup-update-buffer-list ()
  (setq ef-popup-buffer-list
        (append ef-popup-buffer-list
                (cl-set-difference (ef-popup-buffers) ef-popup-buffer-list))))

(add-hook 'window-configuration-change-hook #'ef-popup-update-buffer-list)

(defun ef-popup-find-window (buf)
  (cl-find-if #'(lambda (v)
                  (eq (window-buffer v)
                      buf))
              (ef-popup-windows)))

(defun ef-popup-killed-buffer-hook ()
  "If an open popup window containing the buffer exists, check if more than
one pop up window is in the list. If there is, cycle to it, otherwise delete
the popup window."
  (let ((buf (current-buffer)))
    (if-let ((win (ef-popup-find-window buf)))
        (if (> (length ef-popup-buffer-list) 1)
            (ef-popup-cycle-backward)
          (delete-window win)))

    (setq ef-popup-buffer-list
          (remove buf ef-popup-buffer-list))))

(add-hook 'kill-buffer-hook #'ef-popup-killed-buffer-hook)

(defadvice shackle-display-buffer-action (around ef-single-popup activate)
  "If the newly opened window is a popup window, check if we already
have an open popup. If we do, call `delete-window' on the popup window
before opening a new one."
  (if (and (> (length (window-list)) 1)
           (ef-popup-buffer-p buffer))
      (when-let* ((open-popups (ef-popup-windows))
                  (open-popup (car open-popups)))
        (delete-window open-popup)
        (set-window-dedicated-p ad-do-it t))
    ad-do-it))

(defadvice quit-window (around ef-popup-quit-window activate)
  (unless (ef-popup-buffer-p (current-buffer))
    ad-do-it))

(general-define-key
 :states '(normal insert visual motion replace)
 :keymaps 'override
 "M-h" 'ef-popup-cycle-backward
 "M-l" 'ef-popup-cycle-forward
 "M-p" 'ef-popup-toggle)

(provide 'core-shackle)
