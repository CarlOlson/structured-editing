
(defvar se-highlighted nil
  "Variable for internal usage in se-mode.")

(defvar se-not-highlighted nil
  "Variable for internal usage in se-mode.")

(defvar se-parse-tree nil
  "Variable for internal usage in se-mode.")

(define-minor-mode se-mode
  "Toggle Structure Editing mode.
\\{se-mode-map}"
  :init-value nil
  :lighter " se"
  :keymap (make-sparse-keymap)
  (unless se-mode
    (set (make-local-variable 'se-highlighted) nil)
    (set (make-local-variable 'se-not-highlighted) nil)
    (set (make-local-variable 'se-parse-tree) nil))
  (when se-mode
    (se-clear-highlighted)))

(define-key se-mode-map (kbd "C-c e") 'se-expand-highlighted-span)
(define-key se-mode-map (kbd "C-c s") 'se-shrink-highlighted-span)
(define-key se-mode-map [remap narrow-to-region] 'se-narrow-to-highlighted)

(defun se-set-spans ()
  (interactive)
  (when (null mark-active)
    (se-clear-highlighted))
  (cond
   ((null se-parse-tree)
    'error)
   ((and (null se-highlighted)
	 (null se-not-highlighted))
    (setq se-not-highlighted
	  (nreverse
	   (find-min-span-path (point) se-parse-tree))))))

(defun se-expand-highlighted-span ()
  "In se-mode, highlights smallest span around point. If a region
is already highlighted, it is expanded to its parent region."
  (interactive)
  (se-set-spans)
  (cond
   ((null se-not-highlighted)
    (se-mark-region (point-min) (point-max)))
   (:else
    (push (pop se-not-highlighted) se-highlighted)
    (let ((span (first se-highlighted)))
      (se-mark-region (span-start span) (span-end span))))))

(defun se-shrink-highlighted-span ()
  "In se-mode, unhighlights current region. If a smaller region
was previous highlighted, highlight it again."
  (interactive)
  (se-set-spans)
  (when se-highlighted
    (push (pop se-highlighted) se-not-highlighted)
    (let ((span (first se-highlighted)))
      (when span
	(se-mark-region (span-start span) (span-end span)))))
  (when (null se-highlighted)
    (se-clear-highlighted)))

(defun se-narrow-to-highlighted ()
  (interactive)
  (cond
   ((null se-highlighted)
    (narrow-to-region (point) (mark)))
   (:else
    (let ((span (first se-highlighted)))
      (narrow-to-region (span-start span) (span-end span))))))

(defun se-mark-region (start end)
  (goto-char end)
  (set-mark-command nil)
  (goto-char start))

(defun se-clear-highlighted ()
  "Clears all highlighted regions."
  (interactive)
  (setq se-highlighted nil
	se-not-highlighted nil)
  (when mark-active
    (set-mark-command nil)))
