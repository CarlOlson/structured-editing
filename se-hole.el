
;; Code inspired by Agda2-mode's goals.

(defvar se-hole-open-delimiter "{"
  "Characters to show as the opening of a hole.")

(defvar se-hole-close-delimiter " }"
  "Characters to show as the ending of a hole.")

(defvar se-hole-syntax '("{!" . "!}")
  "Pair of strings to mark beginning and end of holes. These will
be hidden and the user will see `se-hole-open-delimiter' and
`se-hole-close-delimiter' instead.")

(defvar se-hole-hook nil
  "Function hooks to be called when a hole is created. Four
arguments are given as points: INNER-START INNER-END
DELIMITER-START DELIMITER-END. Be sure to include undo
information if necessary.")

(defun se-hole-insert (POINT)
  "Inserts hole at POINT."
  (interactive "d")
  (se-hole-create POINT POINT ""))

(defun se-hole-match (START END)
  "Match existing hole from `se-hole-syntax' in region and setup
text properties."
  (interactive "r")
  (let ((INNER-START (se-hole-match-open-delimiter  START END))
	(INNER-END   (se-hole-match-close-delimiter START END)))
    (unless (and INNER-START INNER-END)
      (error "Could not match hole"))
    (let ((OUTER-START (se-hole--open-delimiter-start INNER-START))
	  (OUTER-END   (se-hole--close-delimiter-end  INNER-END)))
      (se-hole--set-text-props INNER-START INNER-END)
      (se-hole--push-undo-props OUTER-START INNER-START)
      (se-hole--push-undo-props INNER-END   OUTER-END)
      (run-hook-with-args 'se-hole-hook
			  INNER-START INNER-END
			  OUTER-START OUTER-END))))

(defun se-hole-create (START END &optional REPLACE)
  "Insert a hole at position START. Inserts hole over region if
END is non-nil. Region is placed into hole unless REPLACE is
non-nil. If REPLACE is a string it will be used as replacement
text."
  (interactive "r")
  (cond
   ((or (null START) (null END))
    (error "Could not create hole"))
   ((stringp REPLACE)
    ;; replace region, setup hole
    (delete-region START END)
    (insert (car se-hole-syntax)
	    REPLACE
	    (cdr se-hole-syntax))
    (se-hole-match START (point)))
   ((null REPLACE)
    ;; keep region, setup hole
    (se-hole-create START END (buffer-substring START END)))
   (REPLACE
    ;; destroy region, setup hole
    (se-hole-create START END ""))))

(defun se-hole-remove-props (START END)
  "Will indiscriminately remove properties set by `se-hole-create'."
  (interactive "r")
  (let ((inhibit-modification-hooks t))
    (remove-text-properties START END (list 'display nil
					    'rear-nonsticy nil
					    'modification-hooks nil))))

(defun se-hole-match-open-delimiter (START END)
  "Matches end of starting delimiter of `se-hole-syntax' in
region. Returns nil if not matched."
  (save-excursion
    (goto-char START)
    (search-forward (car se-hole-syntax) END t nil)))

(defun se-hole-match-close-delimiter (START END)
  "Matches start of ending delimiter of `se-hole-syntax' in
region. Returns nil if not matched."
  (save-excursion
    (goto-char END)
    (search-backward (cdr se-hole-syntax) START t nil)))

(defun se-hole--push-undo-props (START END)
  (push `(apply se-hole-remove-props ,START ,END)
	buffer-undo-list))

(defun se-hole--set-text-props (START END)
  (add-text-properties (se-hole--open-delimiter-start START) START
		       (list 'display se-hole-open-delimiter
			     'rear-nonsticky t
			     'modification-hooks '(se-hole--signal-read-only)))
  (add-text-properties (se-hole--close-delimiter-end END) END
		       (list 'display se-hole-close-delimiter
			     'rear-nonsticky t
			     'modification-hooks '(se-hole--signal-read-only))))

(defun se-hole--signal-read-only (&rest BEG END args)
  (signal 'text-read-only nil))

(defun se-hole--open-delimiter-start (END)
  (- END (length (car se-hole-syntax))))

(defun se-hole--close-delimiter-end (START)
  (+ START (length (cdr se-hole-syntax))))

(provide 'se-hole)
