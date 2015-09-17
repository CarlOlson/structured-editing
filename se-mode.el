
(make-variable-buffer-local
 (defvar se-mode-selected nil
   "Variable for internal usage in se-mode."))

(make-variable-buffer-local
 (defvar se-mode-not-selected nil
   "Variable for internal usage in se-mode."))

(make-variable-buffer-local
 (defvar se-mode-parse-tree nil
   "Variable for internal usage in se-mode."))

(make-variable-buffer-local
 (defvar se-mode-inspect-hook nil
   "Evaluates hooks when `se-mode-inspect' is called."))

(make-variable-buffer-local
 (defvar se-mode-indent-size 2
   "Indentation size in spaces."))

(define-minor-mode se-mode
  "Toggle Structure Editing mode.
\\{se-mode-map}"
  :init-value nil
  :lighter " se"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "M-s") #'se-navigation-mode)
	    map))

(defvar se-mode-nothing-map
  (let* ((char-table (make-char-table 'keymap))
	 (keymap `(keymap ,char-table)))
    ;; all printable characters
    (set-char-table-range char-table (cons ?\s 255) #'se-mode-nothing)
    ;; tab, backspace, enter
    (define-key keymap (kbd "<tab>") #'se-mode-nothing)
    (define-key keymap (kbd "DEL") #'se-mode-nothing)
    (define-key keymap (kbd "RET") #'se-mode-nothing)
    ;; prevent quoted inserts
    (define-key keymap [remap quoted-insert] #'se-mode-nothing)
    keymap)
  "A keymap to make a buffer weakly read-only.")
  
(define-minor-mode se-navigation-mode
  :init-value nil
  :lighter " navi"
  :keymap (let ((map (make-sparse-keymap se-mode-nothing-map)))
	    (define-key map (kbd "c") #'se-inf-parse-file)
	    (define-key map (kbd "q") (lambda () (interactive) (se-navigation-mode -1)))
	    (define-key map (kbd "e") #'se-mode-expand-selected)
	    (define-key map (kbd "s") #'se-mode-shrink-selected)
	    (define-key map (kbd "i") #'se-mode-inspect)
	    (define-key map (kbd "p") #'se-mode-select-previous)
	    (define-key map (kbd "n") #'se-mode-select-next)
	    map))

(defun se-mode-nothing ()
  (interactive))

(defun se-mode-selected ()
  (first se-mode-selected))

(defun se-mode-clear-selected ()
  "Clears all selected regions."
  (interactive)
  (setq se-mode-selected nil
	se-mode-not-selected nil)
  (when mark-active
    (set-mark-command nil)))

(defun se-mode-set-spans ()
  (when (null mark-active)
    (se-mode-clear-selected))
  (cond
   ((null se-mode-parse-tree)
    'error)
   ((and (null se-mode-selected)
	 (null se-mode-not-selected))
    (setq se-mode-not-selected
	  (nreverse ;; non-destructive se methods should return new lists
	   (se-find-point-path (point) se-mode-parse-tree))))))

(defun se-mode-mark-region (start end)
  (goto-char end)
  (set-mark-command nil)
  (goto-char start))

(defun se-mode-select (term)
  (se-mode-mark-region (se-term-start term) (se-term-end term)))

(defun se-mode-update (term)
  (let ((path (se-find-span-path term se-mode-parse-tree)))
    (setq se-mode-selected nil
	  se-mode-not-selected (reverse path))))

(defun se-mode-expand-selected ()
  "In se-mode, selects smallest span around point. If a region is
already selected, it is expanded to its parent region."
  (interactive)
  (se-mode-set-spans)
  (cond
   ((null se-mode-not-selected)
    (se-mode-mark-region (point-min) (point-max)))
   (:else
    (push (pop se-mode-not-selected) se-mode-selected)
    (se-mode-select (se-mode-selected)))))

(defun se-mode-shrink-selected ()
  "In se-mode, deselect current region. If a smaller region was
previous selected, select it again."
  (interactive)
  (se-mode-set-spans)
  (when se-mode-selected
    (push (pop se-mode-selected) se-mode-not-selected))
  (if se-mode-selected
      (se-mode-select (se-mode-selected))
    (se-mode-clear-selected)))

(defun se-mode-previous ()
  (let ((selected (se-mode-selected))
	(nodes (if se-mode-not-selected
		   (se-node-children (first se-mode-not-selected))
		 se-mode-parse-tree)))
    (loop for (first second . rest) on nodes
	  when (null second) return nil
	  when (se-term-equal-p second selected) return first)))

(defun se-mode-select-previous ()
  "Selects previous node in parse tree."
  (interactive)
  (se-mode-set-spans)
  (let ((prev (se-mode-previous)))
    (cond
     (prev
      (se-mode-update prev)
      (se-mode-expand-selected))
     ;; (se-mode-not-selected
     ;;  (se-mode-expand-selected))
     (:else
      (message "Selected term has no previous.")))))

(defun se-mode-next ()
  (let ((selected (se-mode-selected))
	(nodes (if se-mode-not-selected
		   (se-node-children (first se-mode-not-selected))
		 se-mode-parse-tree)))
    (loop for (first second . rest) on nodes
	  when (null second) return nil
	  when (se-term-equal-p first selected) return second)))

(defun se-mode-select-next ()
  "Selects next node is parse tree."
  (interactive)
  (se-mode-set-spans)
  (let ((next (se-mode-next)))
    (cond
     (next
      (se-mode-update next)
      (se-mode-expand-selected))
     ;; ((and (se-mode-selected)
     ;; 	   (se-node-children (se-mode-selected)))
     ;;  (se-mode-update (first (se-node-children (se-mode-selected))))
     ;;  (se-mode-expand-selected))
     ;; add ability to go around
     (:else
      (message "Selected term has no next.")))))

(defun se-mode-select-name (NAME)
  "Selects the first span named NAME. Starts at current node
selection and moves through parents."
  (se-mode-set-spans)
  (let ((found (find NAME se-mode-not-selected :key #'se-term-name :test #'string=)))
    (when found
      (se-mode-update found)
      (se-mode-select found)
      (se-mode-expand-selected))
    found))

(defun se-mode-popup-window (name text)
  (when (get-buffer name)
    (kill-buffer name))
  (let* ((popup-buffer (get-buffer-create name))
	 (popup-window (display-buffer popup-buffer)))
    (with-current-buffer popup-buffer
      (erase-buffer)
      (insert text)
      (shrink-window-if-larger-than-buffer popup-window))))

(defun se-mode-convert (s)
  (typecase s
    (string s)
    (symbol
     (format "%s" (funcall s (se-mode-selected))))
    (t "")))

(defun se-mode-inspect ()
  "Should displays information on currently selected term. Uses
default method when `se-mode-inspect-hook' is nil, otherwise
evaluates hooks."
  (interactive)
  (cond
   ((null (se-mode-selected)))
   ((null se-mode-inspect-hook)
    (se-mode-popup-window
     "*se*"
     (se-mode-pretty-term (se-mode-selected))))
   (:else
    (run-hooks 'se-mode-inspect-hook)))
  (setq deactivate-mark nil))

(defun se-mode-pretty-json (json)
  (let (max fstr)
    (loop for (key . value) in json
	  maximizing (length (format "%s" key)) into maxlen
	  finally (setq max maxlen))
    (setq fstr (format "%%%ds:\t%%s\n" max))
    (loop for (key . value) in json
	  do (setq key (capitalize (format "%s" key)))
	  collecting (format fstr key value) into lines
	  finally (return (apply #'concat lines)))))

(defun se-mode-pretty-term (term)
  (se-mode-pretty-json 
   (append
    `((name . ,(se-term-name term))
      (start . ,(se-term-start term))
      (end . ,(se-term-end term)))
    (se-span-data (se-first-span term)))))

(defun se-mode-indentable-p (TERM)
  "Returns indentable value of TERM, or nil if one doesn't
exist."
  (let* ((span (se-first-span TERM))
	 (data (se-span-data span)))
    (equal 't (cdr (assoc 'indentable data)))))

(defun se-mode-term-count-lines (term)
  "Counts lines spanned by term. Always returns at least one."
  (1+ (abs (- (line-number-at-pos (se-term-end term))
	      (line-number-at-pos (se-term-start term))))))

(defun se-mode-indent-buffer ()
  "Experimental feature. Indents current buffer.
`se-mode-parse-tree' should have updated span information."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (se-each-line
     (lambda ()
       (let* ((path (se-find-point-path (point) se-mode-parse-tree))
	      (indent-depth (count-if #'se-mode-indentable-p path))
	      (indent-start nil))
	 ;; don't indent the start of indentable spans
	 (loop for node in path
	       when (and (= (point) (se-term-start node))
			 (se-mode-line-start-p (se-term-start node))
			 (se-mode-indentable-p node))
	       do (progn
		    (decf indent-depth)
		    (setq indent-start t)
		    (return)))
	 ;; don't indent if end of indentable span
	 (when (not indent-start)
	   (loop for node in path
		 when (and (se-mode-indentable-p node)
			   (not (se-same-line-p (se-term-start node)
						(se-term-end node)))
			   (se-same-line-p (point) (se-term-end node)))
		 do (progn
		      (decf indent-depth)
		      (return))))
	 ;; indent
	 (when (> indent-depth 0)
	   (indent-line-to (* indent-depth se-mode-indent-size))))))))
