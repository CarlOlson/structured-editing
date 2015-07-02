
(defvar se-mode-selected nil
  "Variable for internal usage in se-mode.")

(defvar se-mode-not-selected nil
  "Variable for internal usage in se-mode.")

(defvar se-mode-parse-tree nil
  "Variable for internal usage in se-mode.")

(define-minor-mode se-mode
  "Toggle Structure Editing mode.
\\{se-mode-map}"
  :init-value nil
  :lighter " se"
  :keymap (make-sparse-keymap)
  (unless se-mode
    (set (make-local-variable #'se-mode-selected) nil)
    (set (make-local-variable #'se-mode-not-selected) nil)
    (set (make-local-variable #'se-mode-parse-tree) nil))
  (when se-mode
    (se-mode-clear-selected)))

(define-key se-mode-map (kbd "C-c e") #'se-mode-expand-selected)
(define-key se-mode-map (kbd "C-c s") #'se-mode-shrink-selected)

;; @TODO implement tree ajusting when region changes
(defalias #'se-mode-narrow-to-selected #'narrow-to-region)
(define-key se-mode-map [remap narrow-to-region] #'se-mode-narrow-to-selected)

(defun se-mode-clear-selected ()
  "Clears all selected regions."
  (interactive)
  (setq se-mode-selected nil
	se-mode-not-selected nil)
  (when mark-active
    (set-mark-command nil)))

(defun se-mode-set-spans ()
  (interactive)
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
    (se-mode-select (first se-mode-selected)))))

(defun se-mode-shrink-selected ()
  "In se-mode, deselect current region. If a smaller region was
previous selected, select it again."
  (interactive)
  (se-mode-set-spans)
  (when se-mode-selected
    (push (pop se-mode-selected) se-not-selected)
    (se-mode-select (first se-mode-selected)))
  (when (null se-mode-selected)
    (se-mode-clear-selected)))

(defun se-mode-popup-window (name text)
  (let* ((popup-buffer (get-buffer-create name))
	 (popup-window (display-buffer popup-buffer)))
    (with-current-buffer popup-buffer
      (erase-buffer)
      (insert text)
      (shrink-window-if-larger-than-buffer popup-window))))
