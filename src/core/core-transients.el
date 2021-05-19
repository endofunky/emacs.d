(require 'core-lib)

(transient-define-prefix ef-buffer-kill-menu ()
  "Kill buffer menu"
  ["Actions"
   [("a" "Kill all other buffers" ef-kill-other-buffers)
    ("o" "Kill other file buffers" ef-kill-all-other-buffers)
    ("s" "Kill some buffers" kill-some-buffers)]
   [:if-not (lambda () (ef-popup-buffer-p (current-buffer)))
            ("k" "Kill buffer" kill-current-buffer)
            ("K" "Kill buffer+window" kill-buffer-and-window)]])

(transient-define-prefix ef-buffer-menu ()
  "Buffer menu"
  [["Actions"
    ("b" "Bury buffer" bury-buffer)
    ("i" "Open ibuffer" ibuffer)
    ("N" "New buffer" evil-buffer-new)
    ("r" "Revert buffer" revert-buffer)]
   ["Navigation"
    ("n" "Next buffer" next-buffer)
    ("p" "Previous buffer" previous-buffer)]]
  ["Menus"
   [("k" "Kill" ef-buffer-kill-menu)]])

(general-define-key
 :states 'normal :prefix ef-prefix
 "b" 'ef-buffer-menu)

(provide 'core-transients)
