;;; oeis.el --- helpers for the Online Encyclopedia of Integer Sequences

;; Copyright 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2020 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 11
;; Keywords: data, OEIS, mathematics
;; URL: http://user42.tuxfamily.org/oeis-el/index.html
;;
;; oeis.el is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; oeis.el is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; This is some helper commands for the Online Encyclopedia of Integer
;; Sequences (OEIS), http://oeis.org
;;
;;     M-x oeis-browse-anum        goto sequence web page
;;     M-x oeis-ffap-enable        for M-x ffap A-number at point
;;     M-x oeis-search             web site search
;;     M-x oeis-superseeker-mail   superseeker search
;;     M-x oeis-download           download html, b-file, etc
;;     M-x oeis-grep               search local stripped file
;;
;; Sequence web pages and b-files are first sought in `oeis-local-directories'
;; so as to go to local downloaded copies before the web site.

;;; Emacsen:

;; Designed for Emacs 24.  Mostly works in XEmacs 21.

;;; Install:
;;
;; To make `M-x oeis-browse-anum' available put oeis.el in one of your
;; `load-path' directories and the following in your .emacs
;;
;;     (autoload 'oeis-browse-anum "oeis" nil t)
;;
;; Similarly the other commands.  To have oeis-ffap-enable on first M-x ffap
;; use
;;
;;     (autoload 'oeis-ffap-enable "oeis" nil t)
;;     (eval-after-load "ffap" '(oeis-ffap-enable))
;;
;; The ~/OEIS/A999999.internal.txt files which can be downloaded by
;; `oeis-download' are utf8 but sometimes Emacs doesn't recognise that on
;; its own.  It can be helped with
;;
;;     (modify-coding-system-alist 'file
;;       "/A[0-9]\\{6,7\\}\\.internal\\.txt\\'" 'utf-8)
;;
;; There's autoload cookies for the functions and for this coding system if
;; you install by `M-x package-install' or know how to use
;; `update-file-autoloads'.
;;

;;; History:

;; Version 1 - the first version
;; Version 2 - correction to re-download check
;; Version 3 - better number list at point
;; Version 4 - strip "+" signs from number list at point
;; Version 5 - allow more punctuation around A-number at point
;; Version 6 - allow +/- on decimal digits at point
;; Version 7 - strip some whitespace for convenience
;; Version 8 - oeis-download cross-ref OEIS terms of use
;; Version 9 - tweak some docstrings
;; Version 10 - fix missing superseeker time check
;; Version 11 - new oeis-base-url, coding system of .internal.txt

;;; Code:

(require 'thingatpt)

(eval-when-compile
  (unless (and (fboundp 'dolist)
               (fboundp 'ignore-errors)
               (fboundp 'pop))
    (require 'cl))) ;; for macros

;; quieten byte compiler
(defvar ffap-alist)           ;; in ffap.el
(defvar grep-command)         ;; in grep.el
(defvar url-unreserved-chars) ;; in url.el or url-util.el

;; Emacs often guesses utf-8 correctly itself, but have seen it missing some
;; A999999.internal.txt files.  Those filenames are per downloads here, so
;; it seems appropriate to have a setup here.  The OEIS names file is a
;; bonus.
;;
;;;###autoload
(when (memq 'utf-8 (coding-system-list))
  (modify-coding-system-alist 'file "/A[0-9]\\{6,7\\}\\.internal\\.txt\\'" 'utf-8)
  (modify-coding-system-alist 'file "/OEIS/names\\'" 'utf-8))


;;-----------------------------------------------------------------------------

;;;###autoload
(defgroup oeis nil
  "Online Encyclopedia of Integer Sequences."
  :prefix "oeis-"
  ;; Is group `external' good?  It's meant for external programs.
  ;; `communications' might be closer, but no-one would look there for
  ;; mathematics.
  :group 'external
  :link  `(url-link :tag "OEIS home page"
                    ,(if (boundp 'oeis-base-url)
                         oeis-base-url "http://oeis.org"))
  :link  '(url-link :tag "oeis.el home page"
                    "http://user42.tuxfamily.org/oeis-el/index.html"))

(defcustom oeis-local-directories
  '("~/OEIS")
  "Local directories for OEIS downloaded files."
  :type  '(repeat directory)
  :group 'oeis)

(defcustom oeis-base-url
  "http://oeis.org"
  "Base URL for OEIS searches and downloads.
Set this to \"https://oeis.org\" if desired for privacy or for
security if you might execute downloaded code snippets.

If you're a registered OEIS user and might be logged-in from the
`browse-url' browser, then set https so you don't send your login
cookie in plain text."
  :type  'string
  :group 'oeis)

(defcustom oeis-search-format "short"
  "Results format for `oeis-search'.
The choices are
    \"long\"    full sequence info
    \"short\"   name, values, and matched text
    \"data\"    name, values

When matching numbers, \"short\" and \"data\" are the same.
When matching text, \"short\" also shows the paragraphs
containing the matched text."

  ;; sample URLs showing the difference between short and data
  ;; https://oeis.org/search?fmt=short&q=bloodshed
  ;; https://oeis.org/search?fmt=data&q=bloodshed

  :type '(radio (const "short")
                (const "long")
                (const "data")))

(defcustom oeis-download-parts '(html)
  "OEIS sequence parts to download.
This is a list of one or more symbols

   html              A123456.html (main sequence page)
   b-file            b123456.txt
   a-file            a123456.txt (if it exists)
   internal-text     A123456.internal.txt

`a-file' and `b-file' are quietly ignored if the sequence doesn't
have them.  (All sequences usually have a b-file generated from
the sample values if nothing bigger.)"

  :type '(set (const html)
              (const b-file)
              (const a-file)
              (const internal-text)))

(defvar oeis-anum-history nil
  "History list of A-numbers entered by the user.
Used by `oeis-browse-anum' and similar.
See `oeis-read-anum' for the main A-number reader.")

(defvar oeis-search-history nil
  "History list of OEIS searched text or numbers.
Used by `oeis-search' and `oeis-grep'.")

;;----------------------------------------------------------------------------
;; compatibility

;; [same in man-completion.el]
;;
(eval-and-compile ;; quieten emacs byte compiler
  ;; no `eval-when-compile' on this fboundp because in xemacs 21.4.22
  ;; easy-mmode.el (which is define-minor-mode etc) rudely defines a
  ;; replace-regexp-in-string, so a compile-time test is unreliable
  (if (fboundp 'replace-regexp-in-string)
      ;; emacs (21 up)
      (defalias 'oeis--replace-regexp-in-string
        'replace-regexp-in-string)

    ;; xemacs21
    (defun oeis--replace-regexp-in-string
      (regexp rep string &optional fixedcase literal)
      "An internal part of oeis.el.
`replace-regexp-in-string' made available in xemacs.
The FIXEDCASE argument is ignored, case is always fixed."
      (replace-in-string string regexp rep literal))))

(eval-and-compile
  (if (eval-when-compile (fboundp 'locate-user-emacs-file))
      ;; emacs
      (defalias 'oeis--locate-user-emacs-file
        'locate-user-emacs-file)
    ;; xemacs21
    (defun oeis--locate-user-emacs-file (basename)
      "An internal part of oeis.el.
Return the absolute filename of a file under ~/.emacs.d/.
This is GNU Emacs `locate-user-emacs-file' made available in
XEmacs."
      (expand-file-name basename "~/.emacs.d/"))))

;;------------------------------------------------------------------------------
;; filenames

(defun oeis-local-filename (filename)
  "Look for FILENAME in any of `oeis-local-directories'.
FILENAME should be a plain filename with no directory part.
The return has a directory part, or return nil if not found."
  (let ((dirs oeis-local-directories)
        found)
    (while (and dirs (not found))
      (let ((fullname (expand-file-name filename (pop dirs))))
        (if (file-exists-p fullname)
            (setq found fullname))))
    found))

;;-----------------------------------------------------------------------------
;; A-number at point

(eval-when-compile
  (put 'oeis-anum-fixup 'pure t))
(defun  oeis-anum-fixup (str)
  "An internal part of oeis.el.
STR is like \"A12345\" or \"12345\" or \"b123456.txt\".
Return an A-number string like \"A012345\".
Leading zeros are extended or shortened as necessary to 6
digits."

  ;; Strip leading and trailing whitespace as a convenience for cutting and
  ;; pasting.
  (setq str (oeis--replace-regexp-in-string "\\`\\(\\s-\\|[\r\n]\\)+\\|\\(\\s-\\|[\r\n]\\)+\\'" "" str))

  (if (string-match "\\`[ab]\\(.*\\)\\.txt\\'" str)
      (setq str (concat "A" (match-string 1 str))))
  (if (string-match "\\`[0-9]" str)
      (setq str (concat "A" str)))
  (if (and (string-match "0*\\([0-9]+\\)" str)
           (/= 6 (- (match-end 0) (match-beginning 0))))
      (setq str
            (concat (substring    str 0 (match-beginning 0))  ;; pre
                    (make-string  (max 0 (- 6 (- (match-end 1)
                                                 (match-beginning 1))))
                                  ?0)                         ;; zeros
                    (match-string 1 str)                      ;; non-zero
                    (substring    str (match-end 0)))))       ;; post
  str)

(eval-when-compile
  (put 'oeis-anum-at-point-bounds 'side-effect-free t))
(defun  oeis-anum-at-point-bounds ()
  "An internal part of oeis.el.
Return a list (BEG END) which are the extents of an A-number at
point in the current buffer.  If no A-number at point then return
nil.

An a-file or b-file like b123456.txt is also reckoned as an
A-number and its extents returned (the full \"b123456.txt\"
part)."

  ;; Narrow to the current line as a speedup for big buffers.  This limits
  ;; the amount of searching back and forward `thing-at-point-looking-at'
  ;; will do in its work-arounds for the way re-search-backward doesn't
  ;; match across point.
  (save-restriction
    (narrow-to-region (line-beginning-position) (line-end-position))
    (let ((case-fold-search nil)) ;; case sensitive

      ;; " and ' are not always word boundaries.  Match them explicitly to
      ;; allow for strings like 'A000290' etc.
      (or (and (thing-at-point-looking-at
                "\\(\\b\\|[`'\"]\\)\\(A[0-9]\\{2,8\\}\\)\\(\\b\\|[`'\"]\\)")
               (list (match-beginning 2) (match-end 2)))
          (and (thing-at-point-looking-at
                "\\(\\b\\|[`'\"]\\)[ab]\\([0-9]\\{2,8\\}\\)\\.txt\\(\\b\\|[`'\"]\\)")
               (list (match-end 1) (match-end 0)))))))

(defun oeis-anum-at-point ()
  "Return an OEIS A-number at point.
The return is a string like \"A000290\".
An a-file or b-file like \"b123456.txt\" at point gives
\"A123456\" too."
  (let ((bounds (oeis-anum-at-point-bounds)))
    (and bounds
         (oeis-anum-fixup (apply 'buffer-substring-no-properties bounds)))))

;; Maybe this to have (thing-at-point 'oeis-anum).
;; ;;;autoload
;; (put 'oeis-anum 'thing-at-point 'oeis-anum-at-point)
;; (put 'oeis-anum 'bounds-of-thing-at-point 'oeis-anum-at-point-bounds)

;;-----------------------------------------------------------------------------

(eval-when-compile
  (put 'oeis-anum-to-remote-url 'pure t))
(defun  oeis-anum-to-remote-url (anum &optional type)
  "Return OEIS website URL for a given ANUM.
ANUM is a string like \"A123456\".
TYPE is one of the following symbols.  nil or omitted means `html'.

  html             http://oeis.org/A123456
  b-file           http://oeis.org/A123456/b123456.txt
  a-file           http://oeis.org/A123456/a123456.txt
  internal-text    http://oeis.org/search?q=id:A123456&fmt=text
  internal-html    http://oeis.org/A123456/internal"

  (cond ((eq type 'b-file)
         (concat oeis-base-url "/" anum "/b" (substring anum 1) ".txt"))
        ((eq type 'a-file)
         (concat oeis-base-url "/" anum "/a" (substring anum 1) ".txt"))
        ((eq type 'internal-text)
         (concat oeis-base-url "/search?q=id:" anum "&fmt=text"))
        ((eq type 'internal-html)
         (concat oeis-base-url "/" anum "/internal"))
        (t
         (concat oeis-base-url "/" anum))))

(eval-when-compile
  (put 'oeis-anum-to-url 'side-effect-free t))
(defun  oeis-anum-to-url (anum)
  "Return a URL for the given OEIS A-number.
ANUM is a string like \"A000290\".
The return is a local copy if found in `oeis-local-directories',

    file:///home/foo/OEIS/A123456.html.

Otherwise the OEIS web site per `oeis-anum-to-remote-url',

    http://oeis.org/A123456"

  (let ((filename (oeis-local-filename (concat anum ".html"))))
    (if filename
        (concat "file://" filename)
      (oeis-anum-to-remote-url anum))))

;;-----------------------------------------------------------------------------

(defun oeis-read-anum ()
  "Read an OEIS A-number from the user.
The default is an A-number at point, per `oeis-anum-at-point'.
The return is a string usually like \"A000290\" but the user
could enter something else.

For convenience a number entered like \"123\" is expanded to
\"A000123\".  Excess or omitted zeros are fixed so \"A12345\"
becomes \"A012345\"."

  (let* ((default (oeis-anum-at-point))
         (ret (read-string (if default
                               (format "A-number (%s): " default)
                             "A-number: ")
                           nil                ;; initial input
                           'oeis-anum-history ;; history
                           default)))         ;; default
    (oeis-anum-fixup ret)))

;;;###autoload
(defun oeis-browse-anum (anum)
  ;; checkdoc-params: (anum)
  "Display an OEIS sequence web page with `browse-url'.
This is a page such as

    URL `http://oeis.org/A000290'

Interactively the default ANUM is an A-number at point.
When entering a number interactively some fixups are applied so
for example it's enough to enter \"290\" to see \"A000290\"."

  (interactive (list (oeis-read-anum)))
  (browse-url (oeis-anum-to-url anum)))

;;------------------------------------------------------------------------------
;; ffap of A-number

;; `ffap-string-at-point' on something like 2*A000290 gives the whole
;; "2*A000290", whereas want just the A-number part.
;;
(defadvice ffap-string-at-point (around oeis disable)
  "Recognise 2*A000290 as an A-number."

  ;; Emacs21 `unload-feature' removes the `oeis-anum-at-point-bounds' code
  ;; but doesn't run our `oeis-unload-function' to remove the defadvice here.
  ;; Defang the code here with an fboundp.
  ;; But without also defanging the `ffap-alist' entries the check here
  ;; is of limited use.
  (let ((bounds (and (fboundp 'oeis-anum-at-point-bounds)
                     (oeis-anum-at-point-bounds))))
    (if bounds
        (progn
          (setq ffap-string-at-point-region bounds)
          (setq ad-return-value
                (setq ffap-string-at-point ;; and return the value
                      (apply 'buffer-substring-no-properties bounds))))
      ad-do-it)))

(defun oeis-ffap-anum (anum)
  "Return a local file or remote URL for an ANUM like A123456.
This is designed for use as a `ffap-alist' handler.

ANUM is a string like \"A000290\".  The return is a local or
remote URL as per `oeis-anum-to-url'."

  (oeis-anum-to-url anum))

(defun oeis-ffap-abfile (name)
  "Return a local file or remote URL for NAME like a123456.txt or b123456.txt.
This is designed for use as a `ffap-alist' handler.

If you have a local downloaded ~/OEIS/a123456.txt then that
filename is returned, otherwise a remote URL like
http://oeis.org/A123456/a123456.txt.  Local files are sought in
`oeis-local-directories'."

  (or (oeis-local-filename name)
      (oeis-anum-to-remote-url (oeis-anum-fixup name)
                               (if (string-match "\\`b" name)
                                   'b-file 'a-file))))

(defconst oeis-ffap-alist-entries
  '(("\\`[ab][0-9]\\{6,7\\}\\.txt\\'" . oeis-ffap-abfile)
    ("\\`A[0-9]\\{6,7\\}\\'"          . oeis-ffap-anum))
  "An internal part of oeis.el.
List of entries to add to `ffap-alist' in `oeis-ffap-enable'.")

;;;###autoload
(defun oeis-ffap-enable ()
  "Extend `ffap' to recognise A-numbers and a,b-files.
With this enabled `M-x ffap' on for example \"A000290\" offers
either a local or remote URL

    file:///home/foo/OEIS/A000290.html
    http://oeis.org/A000290

Local HTML is offered as a URL rather than plain filename since a
URL makes ffap visit with `browse-url' rather than `find-file'.

Similarly on an a-file or b-file \"b1234546.txt\" offers either

    ~/OEIS/b1234546.txt
    http://oeis.org/A123456/b123456.txt

Local files are sought in `oeis-local-directories'.

See `oeis-browse-anum' to go to any A-number entered (default
A-number at point)."

  (interactive)
  (ad-enable-advice 'ffap-string-at-point 'around 'oeis)
  (ad-activate      'ffap-string-at-point)
  (require 'ffap)
  (dolist (entry oeis-ffap-alist-entries)
    (add-to-list 'ffap-alist entry)))


;;-----------------------------------------------------------------------------
;; download

;;;###autoload
(defun oeis-download (anum)
  ;; checkdoc-params: (anum)
  "Download an OEIS sequence using \"wget\".
`oeis-download-parts' is which parts of the sequence to download,
being HTML, b-file, internal, etc.

The download is done by running the \"wget\" program in a
`shell-command'.  If \"wget\" finishes successfully then the
buffer is killed and the window is closed.

Downloaded files are written to the first directory in
`oeis-local-directories'.  The directory is created if necessary.

b-file or a-file are downloaded by \"wget -N\" which means only
download if changed on the server.  The html or internal-text
parts are re-downloaded every time.  (The oeis.org server has a
Last-Modified date on the html, but wget won't do -N with -O.)

If all the desired parts of the sequence are already downloaded
then you are asked whether to re-download.  If a download is
already in progress for the sequence then you are switched to
that buffer rather than beginning another.

This is slightly rough.  The intention might be to offer
alternatives like \"curl\" or maybe url.el as download methods.
Might like also a size limit on a-files and b-files since many
can approach 1Mbyte.  \"curl\" can do a gzip compressed download,
but circa March 2016 the OEIS server doesn't offer that.

Terms of use for OEIS data are at
URL `http://oeis.org/wiki/The_OEIS_End-User_License_Agreement'
\(Creative Commons Attribution Non-Commercial 3.0 at the time of
writing)."

  (interactive (list (oeis-read-anum)))
  (let* ((buffer-name (concat "*oeis wget " anum "*"))
         (buffer      (get-buffer buffer-name))
         (dir         (car oeis-local-directories))
         (num         (substring anum 1))
         (want        nil))

    (when (buffer-live-p buffer)
      (switch-to-buffer buffer)
      (error "Already downloading %s" anum))

    ;; check if already have all `oeis-download-parts' files
    (dolist (part oeis-download-parts)
      (unless (cond ((eq part 'html)
                     (oeis-local-filename (concat anum ".html")))
                    ((eq part 'internal-text)
                     (oeis-local-filename (concat anum ".internal.txt")))
                    ((eq part 'b-file)
                     (oeis-local-filename (concat "b" num ".txt")))
                    ((eq part 'a-file)
                     (oeis-local-filename (concat "a" num ".txt")))
                    (t
                     t)) ;; unrecognised `part', pretend already have it
        (setq want t)))

    (when (or want
              (y-or-n-p "Already downloaded.  Update? "))
      (make-directory dir t) ;; including parent directories

      (setq buffer (get-buffer-create buffer-name))
      (shell-command
       (concat "set -x;\n"
               (format "cd %s || exit 1;\n"
                       (shell-quote-argument (expand-file-name dir)))
               "EXIT=0;\n"
               (and (memq 'html oeis-download-parts)
                    (format "wget -O %s.html %s || EXIT=1;\n"
                            (shell-quote-argument anum)
                            (shell-quote-argument (oeis-anum-to-remote-url anum))))
               (and (memq 'internal-text oeis-download-parts)
                    (format "wget -O %s.internal.txt %s || EXIT=1;\n"
                            (shell-quote-argument anum)
                            (shell-quote-argument (oeis-anum-to-remote-url anum 'internal-text))))
               (and (or (memq 'b-file oeis-download-parts)
                        (memq 'a-file oeis-download-parts))
                    (concat "wget -N"
                            (and (memq 'b-file oeis-download-parts)
                                 (format " %s"
                                         (shell-quote-argument (oeis-anum-to-remote-url anum 'b-file))))
                            (and (memq 'a-file oeis-download-parts)
                                 (format " %s"
                                         (shell-quote-argument (oeis-anum-to-remote-url anum 'a-file))))
                            " || EXIT=1;\n"))
               "exit $EXIT;\n"
               ;; A trailing "&" so `shell-command' runs asynchronously, but
               ;; ":" so it's not executed by the shell as such.
               ": &")
       buffer)
      (set-process-sentinel (get-buffer-process buffer)
                            'oeis-download-sentinel))))

(defun oeis-download-sentinel (process state)
  ;; checkdoc-params: (process state)
  "An internal part of oeis.el.
The `process-sentinel' function in an `oeis-download' buffer.

If the download finishes successfully then the buffer is killed.
If it is showing in a window then that window is switched to
another `oeis-download' buffer if there is one, or if no more
then kill the window too."

  (when (and (eq (process-status process) 'exit)
             (equal 0 (process-exit-status process)))
    ;; successful download
    (let ((output-buffer (process-buffer process)))
      (when output-buffer

        ;; another-oeis-buffer is the top-most named "*oeis wget A123456*" etc
        (let (another-oeis-buffer)
          (dolist (buffer (buffer-list))
            (setq another-oeis-buffer
                  (or another-oeis-buffer
                      (and (not (eq buffer output-buffer))
                           (string-match "\\`\\*oeis wget A[0-9]*\\*"
                                         (buffer-name buffer))
                           buffer))))

          ;; For windows displaying output-buffer, switch to
          ;; another-oeis-buffer, or no another-oeis-buffer then delete the
          ;; window if it's not the sole window of its frame.
          ;;
          ;; ENHANCE-ME: If a window showing output-buffer is
          ;; window-dedicated-p then might like to kill that window in the
          ;; style of `replace-buffer-in-windows'.  The action here would be
          ;; a kind of variation on `replace-buffer-in-windows' which
          ;; replaced a buffer with another given buffer (rather than the
          ;; way `replace-buffer-in-windows' goes to the next on the top of
          ;; the buffer-list, or some such).
          ;;
          (walk-windows
           (lambda (window)
             (when (eq output-buffer (window-buffer window))
               (cond (another-oeis-buffer
                      (set-window-buffer window another-oeis-buffer))
                     ((> (length (window-list (window-frame window) nil)) 1)
                      ;; not the only window in the frame, so delete window
                      (delete-window window))))
             nil  ;; not the minibuffer
             t))  ;; all frames
          (kill-buffer output-buffer))))))

;;------------------------------------------------------------------------------
;; search

(defun oeis-number-list-at-point (&optional separator)
  "An internal part of oeis.el.
Return a string which is a list of integers at point.
For example if point is on numbers like 1,2,3,4, then return
string \"1,2,3,4\".

The numbers can have either commas or whitespace between them.
The list can cross newlines too (but limited to a few lines).
The return has the given SEPARATOR between each number
irrespective of what is in the buffer.  The default SEPARATOR is
comma \",\"."

  ;; Maybe could allow comment-start-skip when crossing a newline, though
  ;; perhaps only if the originating point is in a comment line similarly.

  (unless separator (setq separator ","))
  (save-restriction
    (narrow-to-region (save-excursion (forward-line -2) (point))
                      (save-excursion (forward-line 2) (point)))
    (and (thing-at-point-looking-at "\\([-+]?\\b[0-9]+\\(\\(,\\|\\s-\\)+[-+]?[0-9]+\\)*\\)\\(,\\|\\b\\)")
         (let ((str (match-string 1)))

           ;; "1.5, 123" gives a match "5, 123" so if match preceded by a
           ;; "." then the first number is a decimal, skip it.
           ;; Note xemacs replace-in-string does the wrong thing on
           ;; "\\`[0-9]+" -> "" so use string-match and substring.
           (and (equal ?. (char-before (match-beginning 0)))
                (string-match "\\`[0-9]+[^0-9]*" str)
                (setq str (substring str (match-end 0))))

           ;; strip "+" signs
           (setq str (oeis--replace-regexp-in-string "\\+" "" str))

           ;; commas and spaces between numbers become single `separator'
           (setq str (oeis--replace-regexp-in-string "[^-0-9]+" separator str))

           ;; skipping first ".5" might leave no numbers at all, return nil
           ;; in that case
           (and (not (equal str ""))
                str)))))

(defun oeis-decimal-digits-at-point (&optional separator)
  "An internal part of oeis.el.
Return a string which is a list of decimal digits at point.
If point is on a number like 12.345 then return its digits as a
string like \"1,2,3,4,5\".
The given SEPARATOR is used between the digits.  The default
SEPARATOR is comma \",\".

Leading zeros are stripped, so 0.0012 gives \"1,2\".  A zero
before the decimal point is optional, so .123 gives \"1,2,3\".
A plus or minus sign \"-1.23\" or \"+1.23\" is not included in
the return."

  ;; Don't think could support a non-"." decimal point unambiguously.
  ;; Something like 12,3456 is either decimal digits or two integers.
  ;; Maybe it could be an option, or decided from locale configuration.

  (unless separator (setq separator ","))
  (save-restriction
    (narrow-to-region (line-beginning-position) (line-end-position))
    (and (thing-at-point-looking-at
          "\\(^\\|[-+=,;/\"`' \t\r\n]\\)\\([0-9]*\\.[0-9]+\\b\\)")
         (let ((str (match-string 2)))
           ;; Strip leading zeros.  Careful not to match an empty string as
           ;; that sends xemacs21.4 replace-in-string into an infinite loop.
           (setq str (oeis--replace-regexp-in-string "\\`0*\\.0*\\|\\`0+"
                                                     ""
                                                     str))
           ;; Separator after each digit.
           ;; Should quote `separator' in the replacement here to be fully
           ;; general, but it's only space or comma for now
           (setq str (oeis--replace-regexp-in-string "\\([0-9]\\)[^0-9]?"
                                                     (concat "\\1" separator)
                                                     str))
           ;; no final comma
           (substring str 0 (- (length separator)))))))

;;;###autoload
(defun oeis-search (str)
  ;; checkdoc-params: (str)
  "OEIS web site search.
Use the OEIS web site search to look for numbers or text.
This is a `browse-url' to, for example,

    http://oeis.org/search?fmt=short&q=monopentapolyhexes

Interactively, the default search is a word, numbers, or decimal
digits, at point.  The usual `\\<minibuffer-local-map>\\[next-history-element]' puts this in the
minibuffer if you want to edit.

Search hints are at URL `http://oeis.org/hints.html'.
Commas \",\" mean consecutive number terms in the order given.
Spaces mean the terms in any order.

    112,839,556       in given order
    112 839 556       in any order

The default for numbers at point is \",\" since usually order
matters.  You can edit in the minibuffer or yank something else.

See `oeis-search-format' to choose short or long results."

  (interactive
   (let ((default (or (oeis-decimal-digits-at-point ",")
                      (oeis-number-list-at-point ",")
                      (word-at-point))))
     (list (read-string (if default (format "OEIS search (%s): " default)
                          "OEIS search: ")
                        nil                  ;; initial input
                        'oeis-search-history ;; history
                        default))))

  ;; `url-hexify-string' is in url-util.el in recent url, but in url.el for
  ;; the version with xemacs21.  (require 'url) works in both.
  ;; Hexifying is necessary if searching for "&", and perhaps "/" too.
  ;;
  ;; `url-unreserved-chars' doesn't include comma "," in recent url.el.
  ;; This conforms to the RFC, but override here so that comma is not
  ;; hexified.  The oeis.org server accepts commas as literals and not
  ;; hexifying them makes the URL less ugly when doing number searches.  Use
  ;; a let-bind since `url-hexify-string' in xemacs21.4 doesn't have an
  ;; ALLOWED-CHARS parameter.
  ;;
  (require 'url) ;; for `url-hexify-string'
  (let ((url-unreserved-chars (cons ?, url-unreserved-chars)))
    (setq str (url-hexify-string str)))
  (browse-url (concat oeis-base-url "/search?fmt=" oeis-search-format
                      "&q=" str)))

;;------------------------------------------------------------------------------
;; superseeker

(defun oeis-button-browse-url (button)
  ;; checkdoc-params: (button)
  "An internal part of oeis.el.
This function is designed for use as the `action' property of a button
on a URL.  Go to the URL with `browse-url'."
  (browse-url (button-label button)))

(defun oeis-superseeker-help-kill ()
  "An internal part of oeis.el.
This function is designed for use from `kill-buffer-hook' in a
superseeker mail buffer.

Kill the \"*OEIS superseeker help*\" buffer and its window.
If there's no such buffer then do nothing."

  (let ((buffer (get-buffer "*OEIS superseeker help*")))
    (when (buffer-live-p buffer)
      (delete-windows-on buffer)
      (kill-buffer buffer))))

(defun oeis-superseeker-help ()
  "An internal part of oeis.el.
Display some help instructions for the OEIS superseeker."

  (let ((buffer (get-buffer "*OEIS superseeker help*")))
    (unless buffer
      (setq buffer (get-buffer-create "*OEIS superseeker help*"))
      (with-current-buffer buffer
        (insert "\
Enter a sequence of numbers to super seek.
Separate with spaces, not commas.

   lookup 1 2 3 4 5 6 7 8 9 10 11 12 13

The search help page is ")
        (insert-button (concat oeis-base-url "/ol.html")
                       'face 'link
                       'help-echo (concat "mouse-2, RET: Follow this link")
                       'action 'oeis-button-browse-url
                       'follow-link t)
        (insert "
and superseeker help    ")
        (insert-button (concat oeis-base-url "/superhelp.txt")
                       'face 'link
                       'help-echo (concat "mouse-2, RET: Follow this link")
                       'action 'oeis-button-browse-url
                       'follow-link t)
        (let ((filename (oeis-local-filename "superhelp.txt")))
          (when filename
            (insert "
Local copy of superhelp.txt is ...
")
            (insert-file-contents filename)))
        (goto-char (point-min))
        (text-mode)
        (view-mode)))

    ;; show in a window, if not already showing on the current frame
    (unless (get-buffer-window buffer)
      (save-selected-window
        (condition-case nil
            ;; emacs two args
            (switch-to-buffer-other-window buffer t) ;; no-record
          (error
           ;; xemacs one arg
           (switch-to-buffer-other-window buffer)))
        (shrink-window-if-larger-than-buffer)))))

;; If you're in the "supergoodguys" list and so have no superseeker limit
;; then disable the check with
;;     (setq oeis-superseeker-limit-seconds nil)
;;
(defvar oeis-superseeker-limit-seconds (+ 3600 10)
  "An internal part of oeis.el.
The number of seconds between superseeks.
This is slightly over 1 hour to allow for small clock inaccuracy.

Change this as necessary to match the superseeker restrictions.
If nil then no check at all.")

(defun oeis-superseeker-record-time ()
  "An internal part of oeis.el.
Record the time an OEIS superseeker message was sent."
  (let ((buffer (find-file-noselect (oeis--locate-user-emacs-file
                                     "oeis-superseeker-time.el"))))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format ";; `decode-time' when last OEIS superseeker lookup was sent\n%S\n"
                      (decode-time)))
      (save-buffer))
    (kill-buffer buffer)))

(defun oeis-superseeker-check-time ()
  "An internal part of oeis.el.
Check the OEIS superseeker lookup time restriction.
Return nil if ok, so either never superseeked, or enough time passed
since last superseek, or user asks to continue anyway.

The last time is recorded in ~/.emacs.d/oeis-superseeker-time.el.
If your system clock is somehow badly wrong then answer \"yes\"
to continue anyway.  Or delete the file."

  (when oeis-superseeker-limit-seconds
    ;; `seconds' is how long until next superseek.
    ;; Zero or negative when already past the restriction.
    (let ((seconds
           (ignore-errors
             (- oeis-superseeker-limit-seconds
                (funcall (if (eval-when-compile (fboundp 'time-to-seconds))
                             ;; emacs incompatible change renamed
                             ;; `time-to-seconds' to `float-time'
                             'time-to-seconds 'float-time)
                         (time-since
                          (with-current-buffer (find-file-noselect
                                                (oeis--locate-user-emacs-file
                                                 "oeis-superseeker-time.el"))
                            (goto-char (point-min))
                            (prog1 (apply 'encode-time (read (current-buffer)))
                              (kill-buffer nil)))))))))
      (when (and seconds
                 (> seconds 0))
        (not (yes-or-no-p (format "Superseeker limit one per hour, next in %d minutes.  Continue? "
                                  (/ (+ seconds 59) 60))))))))

;;;###autoload
(defun oeis-superseeker-mail ()
  "Compose an email to the OEIS superseeker.
This is a `compose-mail' to superseeker@oeis.org pre-filled with
a \"lookup\" command.  Edit or enter target values then send mail
with `C-c C-c' in the usual way.

The superseeker is described at

    URL `http://oeis.org/ol.html'.
    URL `http://oeis.org/superhelp.txt'

A summary of the mail format is shown in a help window.  If you
have superhelp.txt somewhere in `oeis-local-directories' then
it's presented too.

The pre-filled lookup command is a list of numbers or decimal
digits at point.  Edit or replace if necessary before sending.
Change the message Subject if you want a reminder in the reply
about what you seeked.

The superseeker works hard to find matches.  Try a plain
`oeis-search' or a local `oeis-grep' before a super seek.  The
superseeker limits to 1 search per user per hour and will bounce
if asked again too soon.  The command here records the last send
time and warns if another would be too soon.

The last time is in file ~/.emacs.d/oeis-superseeker-time.el so
is preserved across Emacs sessions.  If the time is somehow wrong
then you can delete the file, or say \"yes\" to send anyway.
~/.emacs.d is `user-emacs-directory'.

----
The email uses `compose-mail' so all usual features and
configuration of the chosen `mail-user-agent' are available.

You might like to arrange not to keep copies of sent request
messages since the reply shows the numbers searched anyway.
In Gnus, an expression in `gnus-message-archive-group' can give
nil for no archive.

  (setq gnus-message-archive-group
        '((if (equal (message-fetch-field \"To\")
                     \"superseeker@oeis.org\")
              nil
            \"sent\")))

`gnus-posting-styles' can't do the same since the headers are not
yet in the buffer.  An expression has a dynamic binding of
variable `to' from `compose-mail', but that's not a documented
feature and if it goes lexical then it will be unavailable."

  (interactive)
  (oeis-superseeker-check-time)
  (let ((default (or (oeis-decimal-digits-at-point " ")
                     (oeis-number-list-at-point " ")
                     "")))
    (compose-mail "superseeker@oeis.org" ;; to
                  "super seek please"    ;; subject
                  nil                    ;; other-headers
                  nil                    ;; continue existing send
                  nil                    ;; switch-function
                  nil                    ;; yank-action
                  '((oeis-superseeker-record-time))) ;; send-actions list

    (let ((old-modified (buffer-modified-p)))
      (insert "lookup " default)
      (save-excursion (insert "\n"))
      (set-buffer-modified-p old-modified)))

  (oeis-superseeker-help)
  (if (eval-when-compile (fboundp 'make-local-hook))
      (make-local-hook 'kill-buffer-hook)) ;; for xemacs21
  (add-hook 'kill-buffer-hook 'oeis-superseeker-help-kill
            nil ;; not append
            t)) ;; buffer local

;;------------------------------------------------------------------------------
;; grep of ~/OEIS/stripped

;;;###autoload
(defun oeis-grep (str)
  ;; checkdoc-params: (str)
  "Grep the ~/OEIS/stripped file for a number sequence.
Interactively the default is a list of numbers at point or the
digits of a decimal number at point.  The usual `\\<minibuffer-local-map>\\[next-history-element]' puts
this in the minibuffer if minor editing is needed.

Exactly how this grep works is highly experimental.  Currently if
you have Perl module Math::OEIS::Grep then that's used, otherwise
a plain `grep'.  Math::OEIS::Grep is best as it tries a few
variations on the values if no exact match.

This grep is good when offline.  If online then the OEIS web site
search (`oeis-search') might give either the same or more or less
results (and occasionally a very big list of results).  See
`oeis-superseeker-mail' for a super search if plain search is
unrevealing."

  (interactive
   (let ((default (or (oeis-decimal-digits-at-point)
                      (oeis-number-list-at-point))))
     (list (read-string (if default (format "OEIS grep (%s): " default)
                          "OEIS search: ")
                        nil                  ;; initial input
                        'oeis-search-history ;; history
                        default))))

  ;; Strip whitespace.  Must not have spaces for the plain "grep", and for
  ;; Math::OEIS::Grep it looks better in the compile window without.
  (setq str (oeis--replace-regexp-in-string "\\s-+" "" str))

  ;; Strip leading "+" signs.  Must not have + signs for the plain "grep" as
  ;; it would be a regexp repetition.  Math::OEIS::Grep strips + signs
  ;; automatically but stripping them better shows in the command what is
  ;; searched.
  (setq str (oeis--replace-regexp-in-string "^\\+" "" str))
  (setq str (oeis--replace-regexp-in-string ",\\+" "," str))

  ;; ENHANCE-ME: shell-quote-argument likes to escape commas "," which
  ;; doesn't look very good and ought to be unnecessary with the kind of
  ;; arguments used here.

  (if (equal 0 (ignore-errors (call-process "perl"
                                            nil ;; stdin
                                            nil ;; stdout
                                            nil ;; no redisplay
                                            "-MMath::OEIS::Grep"
                                            "-e"
                                            "exit 0")))
      (progn
        (eval-and-compile (require 'compile))
        (let ((compilation-ask-about-save nil)
              (compilation-buffer-name-function (lambda (mode) "*oeis grep*"))
              ;; don't store in compile-command or compile-history
              compile-command
              compile-history)
          (compile (concat "perl -MMath::OEIS::Grep=-search,"
                           (shell-quote-argument str)))))

    ;; in emacs 22 or 23 up must `grep-compute-defaults' to have
    ;; `grep-command' variable initialized
    (ignore-errors
      (require 'grep)
      (grep-compute-defaults))

    (grep (concat grep-command
                  (shell-quote-argument str)
                  " "
                  (shell-quote-argument (oeis-local-filename "stripped"))))))

;;------------------------------------------------------------------------------

(defun oeis-unload-function ()
  "An internal part of oeis.el.
Remove defadvice from function `ffap-string-at-point'.
This is called by `unload-feature'."
  (when (ad-find-advice 'ffap-string-at-point 'around 'oeis)
    (ad-remove-advice   'ffap-string-at-point 'around 'oeis)
    (ad-activate        'ffap-string-at-point))
  (when (boundp 'ffap-alist) ;; if ffap.el load
    (dolist (entry oeis-ffap-alist-entries)
      (setq ffap-alist (remove entry ffap-alist))))
  nil) ;; and do normal unload-feature actions too

;;------------------------------------------------------------------------------
;; LocalWords: txt oeis Ryde anum ffap superseeker html http org fmt gz wget
;; LocalWords: foo grep minibuffer superseeks superhelp

(provide 'oeis)

;;; oeis.el ends here
