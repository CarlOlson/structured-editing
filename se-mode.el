
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
 (defvar se-mode-inspect-format
   '("Type:\t" se-term-name "\nStart:\t" se-term-start "\nEnd:\t" se-term-end)
   "Format string for use with `se-mode-inspect'. Symbols are
called as methods with the current selected term. Strings are
echoed."))

(make-variable-buffer-local
 (defvar se-navi-pre-state nil
   "Holds buffer state before se-mode is activated for
navigation."))

(define-minor-mode se-mode
  "Toggle Structure Editing mode.
\\{se-mode-map}"
  :init-value nil
  :lighter " se"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "M-s") #'se-navigation-mode)
	    map))

(define-minor-mode se-navigation-mode
  :init-value nil
  :lighter " navi"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "c") #'se-inf-parse-file)
	    (define-key map (kbd "q") (lambda () (interactive) (se-navigation-mode -1)))
	    (define-key map (kbd "e") #'se-mode-expand-selected)
	    (define-key map (kbd "s") #'se-mode-shrink-selected)
	    (define-key map (kbd "i") #'se-mode-inspect)
	    (define-key map (kbd "<left>") #'se-mode-select-previous)
	    (define-key map (kbd "<right>") #'se-mode-select-next)
	    map)
  (when se-navigation-mode
    (se-navi-push-state)
    (read-only-mode 1))
  (unless se-navigation-mode
    (se-navi-pop-state)))

(defun se-navi-state-put (key value)
  (setq se-navi-pre-state
	(plist-put se-navi-pre-state key value)))

(defun se-navi-state-get (key)
  (plist-get se-navi-pre-state key))

(defun se-navi-push-state ()
  (se-navi-state-put 'read-only (if buffer-read-only 1 -1)))

(defun se-navi-pop-state ()
  (read-only-mode (se-navi-state-get 'read-only)))


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

(defun se-mode-popup-window (name text)
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
  "Displays information on currently selected term. Uses
`se-mode-inspect-format' to determine display format."
  (interactive)
  (when (se-mode-selected)
    (se-mode-popup-window
     "*se*"
     (mapconcat #'se-mode-convert se-mode-inspect-format nil))
    (setq deactivate-mark nil)))
