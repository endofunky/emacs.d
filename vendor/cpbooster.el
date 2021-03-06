(require 'vterm)

(defgroup cpbooster nil
  "An Emacs interface for cpbooster."
  :group 'tools
  :group 'convenience
  :prefix "cpbooster-")

(defcustom cpbooster-bin "cpbooster"
  "The command used to run cpbooster."
  :type 'string)

(defun cpbooster--vterm-sentinel (process event)
  "A process sentinel. Kills PROCESS's buffer if it is live."
  (let ((b (process-buffer process)))
    (and (buffer-live-p b)
         (message "cpbooster process finished."))))

(defun cpbooster--run (&rest args)
  (when-let ((buffer (get-buffer "*cpbooster*")))
    (when-let ((win (get-buffer-window buffer)))
      (delete-window win))
    (kill-buffer buffer))

  (let ((vterm-shell (format "%s -c '%s'" vterm-shell
                             (mapconcat 'identity `(,cpbooster-bin ,@args) " ")))
        (vterm-kill-buffer-on-exit nil))
    (with-current-buffer (vterm "*cpbooster*")
      (set-process-sentinel vterm--process #'cpbooster--vterm-sentinel)
      (when (fboundp 'evil-normal-state)
        (evil-normal-state)))))

;;;###autoload
(defun cpbooster-clone ()
  (interactive)
  (cpbooster--run "clone"))

;;;###autoload
(defun cpbooster-test ()
  (interactive)
  (cpbooster--run "test" (buffer-file-name)))

;;;###autoload
(defun cpbooster-debug ()
  (interactive)
  (cpbooster--run "test" "-d" (buffer-file-name)))

(provide 'cpbooster)
