
(require 'tq)
(require 'json)

(eval-when-compile (require 'cl))

(make-variable-buffer-local
 (defvar se-inf-process nil
   "Holds process for current buffer in se-mode. Processes are
started with `start-process'."))

(make-variable-buffer-local
 (defvar se-inf-queue nil
   "Transaction queue for `se-inf-process'."))

(make-variable-buffer-local
 (defvar se-inf-response-hook
   (list #'se-inf-process-spans
	 #'se-inf-process-error
	 #'se-inf-process-error-span)
   "Functions to be evaluated after response of `se-inf-ask',
response given as only argument. If `se-inf-response-is-json' is
non-nil the response is parsed as JSON first."))

(make-variable-buffer-local
 (defvar se-inf-parse-hook (list #'se-inf-save-if-modified #'se-inf-remove-overlays)
   "Functions to be evaluated before parse request."))

(make-variable-buffer-local
 (defvar se-inf-response-is-json t
   "Non-nil if `se-inf-process' should returns json. See
`se-inf-response-hook'."))

(defun se-inf-start (PROC &optional NO-AUTO-KILL)
  "Initialize necessary variables to use se-inf
functions. Expects PROC to be the process returned from
`start-process'. Should be called at the start of an
`se-mode'.

When NO-AUTO-KILL is nil the user will not be queried about PROC
upon exiting emacs."
  (unless (process-get PROC 'se-inf-queue)
    (process-put PROC 'se-inf-queue (tq-create PROC))
    (process-put PROC 'se-inf-auto-kill (not NO-AUTO-KILL)))
  (setq
   se-inf-process PROC
   se-inf-queue (process-get PROC 'se-inf-queue)))

(defun se-inf-stop ()
  "Should be called at the end of an `se-mode'. This will kill
the process, should be skipped if process is shared."
  (tq-close se-inf-queue)
  (kill-buffer (tq-buffer se-inf-queue)))

(defun se-inf-ask (question &optional FN)
  "Send a message to the current `se-inf-process'. Question will
be terminated with a new line. Calls FN or
`se-inf-process-response' when a one line response is returned."
  (unless (string-suffix-p "\n" question)
    (setq question (concat question "\n")))
  (tq-enqueue se-inf-queue question "\n" (buffer-name) (or FN #'se-inf-process-response)))

(defun se-inf-process-response (CLOSURE RESPONSE)
  (condition-case err
      (with-current-buffer CLOSURE
	(if se-inf-response-is-json
	    (let* ((json-array-type 'list)
		   (json (json-read-from-string RESPONSE)))
	      (run-hook-with-args 'se-inf-response-hook json))
	  (run-hook-with-args 'se-inf-response-hook RESPONSE)))
    (error
     (message "%s" (error-message-string err)))))

(defun se-inf-parse-file (&rest file)
  "Sends parse request to current process. Sends the default
request unless `se-inf-parse-hook' is non-nil. Uses the current
buffer's file unless `file' is non-nil."
  (interactive)
  (run-hooks 'se-inf-parse-hook)
  (se-inf-ask (or file (buffer-file-name))))

(defun se-inf-save-if-modified ()
  "Save the buffer only if it is modified."
  (interactive)
  (when (buffer-modified-p)
    (save-buffer)))

(defun se-inf-get-spans (json)
  (cl-labels ((new-span (lst) ;; emacs 24.3 made `labels' obsolete
			(apply #'se-new-span lst)))
    (mapcar #'new-span (cdr (assoc 'spans json)))))

(defun se-inf-process-spans (json)
  (when (se-inf-get-spans json)
    (setq se-mode-parse-tree
	  (se-create-parse-tree (se-inf-get-spans json)))))

(defun se-inf-get-error (json)
  (cdr (assoc 'error json)))

(defun se-inf-process-error (json)
  (let ((msg (se-inf-get-error json)))
    (when msg
      (message (format "Error: %s" msg)))))

(defun se-inf-get-error-span (json)
  (let ((info (cdr (assoc 'error-span json))))
    (when info
      (apply #'se-new-span info))))

(defun se-inf-process-error-span (json)
  (let ((span (se-inf-get-error-span json)))
    (when span
      (se-inf-error-overlay span)
      (se-mode-goto-term span))))

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

(defun se-inf-kill-emacs-advice (ORIG &optional ARG)
  "Don't query about killing processes if they have
`se-inf-auto-kill' set to a non-nil value."
  (let ((non-auto-kill-procs
	 (cl-remove-if (lambda (proc) (process-get proc 'se-inf-auto-kill)) (process-list))))
    (cl-letf (((symbol-function 'process-list) (lambda () non-auto-kill-procs)))
      (funcall ORIG ARG))))

(if (fboundp #'advice-add)
    (advice-add #'save-buffers-kill-emacs :around #'se-inf-kill-emacs-advice))

(provide 'se-inf)
