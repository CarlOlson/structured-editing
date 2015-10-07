
(require 'tq)
(require 'json)

(make-variable-buffer-local
 (defvar se-inf-process nil
   "Holds process for current buffer in se-mode. Processes are
started with `start-process'."))

(make-variable-buffer-local
 (defvar se-inf-queue nil
   "Transaction queue for `se-inf-process'."))

(make-variable-buffer-local
 (defvar se-inf-process-response
   (lambda (closure response)
     (with-current-buffer closure (se-inf-process-json-string response)))
   "Stores callback function for `se-inf-ask'."))

(make-variable-buffer-local
 (defvar se-inf-parse-hook (list #'save-buffer #'se-inf-remove-overlays)
   "Functions to be evaluated before parse request."))

(defun se-inf-start (PROC)
  "Initialize necessary variables to use se-inf
functions. Expects PROC to be the process returned from
`start-process'. Should be called at the start of an
`se-mode'."
  (unless (process-get PROC 'se-inf-queue)
    (process-put PROC 'se-inf-queue (tq-create PROC)))
  (setq
   se-inf-process PROC
   se-inf-queue (process-get PROC 'se-inf-queue)))

(defun se-inf-stop ()
  "Should be called at the end of an `se-mode'. This will kill
the process, should be skipped if process is shared."
  (tq-close se-inf-queue)
  (kill-buffer (tq-buffer se-inf-queue)))

(defun se-inf-ask (question)
  "Send a message to the current `se-inf-process'. Question will
be terminated with a new line. Calls function stored inside
`se-inf-process-response' when a new line terminated response is
returned."
  (unless (string-suffix-p "\n" question)
    (setq question (concat question "\n")))
  (tq-enqueue se-inf-queue question "\n" (buffer-name) se-inf-process-response))

(defun se-inf-parse-file (&rest file)
  "Sends parse request to current process. Sends the default
request unless `se-inf-parse-hook' is non-nil. Uses the current
buffer's file unless `file' is non-nil."
  (interactive)
  (run-hooks 'se-inf-parse-hook)
  (se-inf-ask (or file (buffer-file-name))))

(defun se-inf-get-spans (json)
  (cl-labels ((new-span (lst) ;; emacs 24.3 made `labels' obsolete
			(apply #'se-new-span lst)))
    (mapcar #'new-span (cdr (assoc 'spans json)))))

(defun se-inf-process-spans (spans json)
  (setq se-mode-parse-tree (se-create-parse-tree spans)))

(defun se-inf-get-error (json)
  (cdr (assoc 'error json)))

(defun se-inf-process-error (msg json)
  (when msg
    (message (format "Error: %s" msg))))

(defun se-inf-get-error-span (json)
  (let ((info (cdr (assoc 'error-span json))))
    (when info
      (apply #'se-new-span info))))

(defun se-inf-process-error-span (span)
  (when span
    (se-inf-error-overlay span)))

(defun se-inf-process-json-string (str)
  (condition-case err
      (let* ((json-array-type 'list)
	     (json (json-read-from-string str)))
	(se-inf-process-spans (se-inf-get-spans json) json)
	(se-inf-process-error (se-inf-get-error json) json)
	(se-inf-process-error-span (se-inf-get-error-span json))
	json)
    (error
     (message "%s" (error-message-string err)))))

(defun se-inf-remove-overlays (&rest args)
  (remove-overlays (point-min) (point-max)))

(defun se-inf-error-overlay (span)
  (let ((overlay (make-overlay (se-term-start span)
			       (se-term-end span))))
    (overlay-put overlay 'info (se-span-data (se-first-span span)))
    (overlay-put overlay 'face "error")
    (overlay-put overlay 'modification-hooks
		 (list (lambda (overlay &rest args)
			 (overlay-put overlay 'face nil))))))

(provide 'se-inf)
