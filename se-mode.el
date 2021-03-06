
(require 'se)
(require 'se-navi)
(require 'se-inf)

(eval-when-compile (require 'cl))

(make-variable-buffer-local
 (defvar se-mode-selected nil
   "The first element is the currently selected span. Most new
methods shouldn't need to touch this variable.  See
`se-mode-not-selected' for more information."))

(make-variable-buffer-local
 (defvar se-mode-not-selected nil
   "Set by `se-mode-set-spans' with the path to the currently
selected point.  Methods like `se-mode-expand-selected' pop
elements into `se-mode-selected' to keep track of the currently
selected span.  Most new methods shouldn't need to touch this
variable."))

(make-variable-buffer-local
 (defvar se-mode-parse-tree nil
   "Variable to hold constructed parse tree for `se-mode'
methods.  Should normally only be accessed from the
`se-mode-parse-tree' function."))

(defvar se-mode-inspect-hook nil
  "Evaluates hooks when `se-mode-inspect' is called.")

(defvar se-mode-last-popup-window nil
  "Holds last window `se-mode-popup-window' created.")

(defvar se-mode-expand-skips-whitespace t
  "When non-nil, before expanding from `se-mode-expand-selected'
move the point to the first non-whitespace character if the point
is currently before that character.")

(defvar se-mode-debug t
  "Log debug information to buffer *se-log*.")

(define-minor-mode se-mode
  "Toggle Structure Editing mode.
\\{se-mode-map}"
  :init-value nil
  :lighter " se"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "M-s") #'se-navigation-mode)
	    map))

(make-obsolete 'se-mode nil)

(define-error 'se-mode-nil-parse-tree
  "Parse tree not set")

(defun se-mode-parse-tree ()
  "Returns the current parse tree if non-nil.  Otherwise, raises
an error."
  (if se-mode-parse-tree
      se-mode-parse-tree
    (signal 'se-mode-nil-parse-tree nil)))

(defun se-mode-selected ()
  "Returns the currently selected span or nil."
  (first se-mode-selected))

(defun se-mode-clear-selected ()
  "Clears all selected regions."
  (interactive)
  (setq se-mode-selected nil
	se-mode-not-selected nil
	mark-active nil))

(defun se-mode-set-spans ()
  "Used by `se-mode' methods to set `se-mode-selected' and
`se-mode-not-selected'."
  (unless mark-active
    (se-mode-clear-selected))
  (when (and (null se-mode-selected)
	     (null se-mode-not-selected))
    (setq se-mode-not-selected
	  (nreverse ;; non-destructive se methods should return new lists
	   (se-find-point-path (point) (se-mode-parse-tree))))))

(defun se-mode-mark-region (start end)
  "Sets mark and point to cover region from START to END. Will be
highlighted if `transient-mark-mode' is on."
  (goto-char start)
  (push-mark end t t)
  (setq deactivate-mark nil))

(defun se-mode-mark-term (term)
  "Calls `se-mode-mark-region' with region covered by TERM."
  (se-mode-mark-region (se-term-start term) (se-term-end term)))

(defun se-mode-skip-beginning-whitespace ()
  "Moves point forward to first non-whitespace character on
current line.  Point doesn't move if already past it."
  (interactive)
  (let (indentation)
    (save-excursion
      (back-to-indentation)
      (setq indentation (point)))
    (when (> indentation (point))
      (goto-char indentation))))

(defun se-mode-rewind-selected ()
  "Push all elements from `se-mode-selected' back onto
`se-mode-not-selected'."
  (while (not (null se-mode-selected))
    (push (pop se-mode-selected) se-mode-not-selected)))

(defun se-mode-expand-selected ()
  "Selects smallest span around point.  If a region is already
selected, it is expanded to its parent region."
  (interactive)
  (when se-mode-expand-skips-whitespace
    (se-mode-skip-beginning-whitespace))
  (se-mode-set-spans)
  (cond
   ((null se-mode-not-selected)
    (se-mode-mark-region (point-min) (point-max)))
   (:else
    (push (pop se-mode-not-selected) se-mode-selected)
    (se-mode-mark-term (se-mode-selected)))))

(defun se-mode-shrink-selected ()
  "Deselect current region.  If a smaller region was previous
selected, select it again."
  (interactive)
  (se-mode-set-spans)
  (when se-mode-selected
    (push (pop se-mode-selected) se-mode-not-selected))
  (if se-mode-selected
      (se-mode-mark-term (se-mode-selected))
    (se-mode-clear-selected)))

(defun se-mode-find-sibling (fn)
  "Specialized find function for `se-mode-previous' and
`se-mode-next'."
  (let ((nodes (if se-mode-not-selected
		   (se-node-children (first se-mode-not-selected))
		 (se-mode-parse-tree))))
    (loop for (head . tail) on nodes
	  when (apply fn head tail)
	  return (cons head tail))))

(defun se-mode-previous ()
  "Return the node before the currently selected one."
  (when (se-mode-selected)
    (nth 0 (se-mode-find-sibling
	    (lambda (prev &optional next &rest _)
	      (and next
		   (se-term-equal-p next (se-mode-selected))))))))

(defun se-mode-next ()
  "Return the node after the currently selected one."
  (when (se-mode-selected)
    (nth 1 (se-mode-find-sibling
	    (lambda (prev &rest _)
	      (se-term-equal-p prev (se-mode-selected)))))))

(defun se-mode-select (term)
  "Updates selection path and selects region."
  (se-mode-set-spans)
  (when term
    (let ((path (se-find-span-path term (se-mode-parse-tree))))
      (setq se-mode-selected nil
	    se-mode-not-selected (reverse path)))
    (se-mode-expand-selected)
    t))

(defun se-mode-select-previous ()
  "Selects previous node in parse tree."
  (interactive)
  (unless (se-mode-select (se-mode-previous))
    (message "Selected term has no previous.")))

(defun se-mode-select-next ()
  "Selects next node is parse tree."
  (interactive)
  (unless (se-mode-select (se-mode-next))
    (message "Selected term has no next.")))

(defun se-mode-select-name (name)
  "Selects the first span named NAME.  Starts at current node
selection and moves through parents."
  (se-mode-set-spans)
  (let ((found (cl-find name se-mode-not-selected :key #'se-term-name :test #'string=)))
    (when found
      (while (not (equal found (se-mode-selected)))
	(se-mode-expand-selected))
      found)))

(defun se-mode-goto-term (term)
  "Centers window at start of TERM."
  (goto-char (se-term-start term))
  (recenter-top-bottom))

(defun se-mode-popup-window (buffer-or-name text)
  "Creates a window to hold TEXT. Handles special options for
setting up the window how `se-mode' wants it."
  (with-temp-buffer-window
   buffer-or-name
   '(display-buffer-below-selected
     . ((window-height . shrink-window-if-larger-than-buffer)))
   #'(lambda (window _) (setq se-mode-last-popup-window window))
   (princ (or text "")))
  (with-current-buffer buffer-or-name
    (special-mode)))

(defun se-mode-inspect-destroy ()
  "Suffix chosen to match default keybinding 'd'."
  (interactive)
  (when (window-valid-p se-mode-last-popup-window)
    (quit-window t se-mode-last-popup-window)))

(defun se-mode-inspect ()
  "Should displays information on currently selected term.  Uses
default method (described in docs) when `se-mode-inspect-hook' is
nil, otherwise evaluates hooks."
  (interactive)
  (se-mode-set-spans)
  (when (get-buffer "*se*")
    ;; buffer is killed for feedback
    (se-mode-inspect-destroy)
    ;; redisplay to flash buffer
    (redisplay))
  (cond
   ((null (se-mode-selected))
    (se-mode-popup-window
     "*se*"
     (se-mode-pretty-print (se-mode-overlay-info-at (point)))))
   ((null se-mode-inspect-hook)
    (se-mode-popup-window
     "*se*"
     (se-mode-pretty-print (se-term-to-json (se-mode-selected)))))
   (:else
    (run-hooks 'se-mode-inspect-hook)))
  (setq deactivate-mark nil))

(defun se-mode-overlay-info-at (start &optional end)
  "Returns the overlay info property in the region from START to
END.  Looks only at START if END is nil."
  (let ((get-info (lambda (overlay)
		    (overlay-get overlay 'info))))
    (apply #'append
	   (mapcar get-info (if end (overlays-in start end)
			      (overlays-at start))))))

(defun se-mode-pretty-print (data &optional depth)
  "Recursively prints a table in a more human readable form."
  (cond
   ((and (consp data)
	 (cl-every #'consp data))
    ;; pairs
    (se--pretty-pairs data (or depth 0)))
   (t
    ;; atoms
    (format "%s" data))))

(defun se--pretty-pairs (pairs depth)
  (let (max fstr)
    (loop for (key . value) in pairs
	  maximizing (length (format "%s" key)) into maxlen
	  finally (setq max maxlen))
    (setq fstr (format "%%%ds:\t%%s\n" max))
    (loop for (key . value) in pairs
	  do (setq key (format "%s" key))
	  collecting (format fstr key
			     (se-mode-pretty-print value (+ max 9))) into lines
	  finally (return (se--pretty-indent (apply #'concat lines) depth)))))

(defun se--pretty-indent (text depth)
  (mapconcat #'identity
	     (split-string text "[\n]")
	     (format "\n%s" (make-string depth ?\s))))

(defun se-term-to-json (term)
  "Converts a term to JSON."
  (append
   `((name . ,(se-term-name term))
     (start . ,(se-term-start term))
     (end . ,(se-term-end term)))
   (se-span-data (se-first-span term))))

(defmacro se-mode-progn (&rest body)
  "Evaluates BODY forms, ensures that se-mode variables (like the
parse tree) stay current after every statement.  To ensure that
multiple statements are executed together use a `progn' or
similar statement in BODY."
  (declare (indent 0) (debug t))
  (let (newbody)
    (dolist (expr body)
      ;; call our helper function before each form
      (push '(se-mode--progn-check-h) newbody)
      (push expr newbody))
    (setq newbody (reverse newbody)) ;; order is backwards
    `(progn
       (unwind-protect
	   (let (se-progn-changed)
	     (add-hook 'first-change-hook #'se-mode--progn-change-h nil t)
	     ,@newbody)
	 (remove-hook 'first-change-hook #'se-mode--progn-change-h t)))))

(defun se-mode--progn-change-h ()
  "Helper function for `se-mode-progn'."
  (setq se-progn-changed t))

(defun se-mode--progn-check-h ()
  "Helper function for `se-mode-progn'."
  (when se-progn-changed
    (se-inf-parse-and-wait)
    (setq se-progn-changed nil)))

(defun se-mode-log (fmt &rest args)
  "Logs a message for debugging purposes."
  (when se-mode-debug
    (with-current-buffer (get-buffer-create "*se-log*")
      (insert (apply #'format fmt args) "\n"))))

(defun se-mode-push-parse-tree (&rest _)
  "Push undo information to restore the current
`se-mode-parse-tree' at a later time.  Sets `se-mode-parse-tree'
to nil.

It is expected that se-modes will add this function to the
`before-change-functions' hook (as `se-mode-create' does)."
  (when se-mode-parse-tree
    (push `(apply set . (se-mode-parse-tree ,se-mode-parse-tree))
	  buffer-undo-list)
    (setq se-mode-parse-tree nil)))

(provide 'se-mode)
