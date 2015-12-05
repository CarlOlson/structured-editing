
(require 'tq)
(require 'json)

(eval-when-compile (require 'cl))

(make-variable-buffer-local
 (defvar se-inf-process nil
   "Holds process for current buffer in se-mode.  Processes are
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
response given as only argument.  If `se-inf-response-is-json' is
non-nil the response is parsed as JSON first."))

(make-variable-buffer-local
 (defvar se-inf-parse-hook (list #'se-inf-save-if-modified #'se-inf-remove-overlays)
   "Functions to be evaluated before parse request."))

(make-variable-buffer-local
 (defvar se-inf-response-is-json t
   "Non-nil if `se-inf-process' should returns JSON.  See
`se-inf-response-hook'."))

(defun se-inf-start (proc &optional no-auto-kill)
  "Initialize necessary variables to use se-inf functions.
Expects PROC to be the process returned from `start-process'.
Should be called at the start of an `se-mode'.

When NO-AUTO-KILL is nil the user will not be queried about PROC
still being active upon exiting emacs."
  (unless (process-get proc 'se-inf-queue)
    (process-put proc 'se-inf-queue (tq-create proc))
    (process-put proc 'se-inf-auto-kill (not no-auto-kill)))
  (setq
   se-inf-process proc
   se-inf-queue (process-get proc 'se-inf-queue)))

(defun se-inf-stop ()
  "Should be called at the end of an `se-mode' session.  This
will kill the process, should be skipped if process is shared."
  (tq-close se-inf-queue)
  (kill-buffer (tq-buffer se-inf-queue)))

(defun se-inf-ask (question &optional fn)
  "Send a message to the current `se-inf-process'.  Question will
be terminated with a new line. Calls FN or
`se-inf-process-response' when a one line response is returned."
  (unless (string-suffix-p "\n" question)
    (setq question (concat question "\n")))
  (tq-enqueue se-inf-queue question "\n" (buffer-name) (or fn #'se-inf-process-response)))

(defun se-inf-process-response (closure response)
  "Called to evaluate `se-inf-response-hook' upon response from
`se-inf-process'."
  (condition-case err
      (with-current-buffer closure
	(if se-inf-response-is-json
	    (let* ((json-array-type 'list)
		   (json (json-read-from-string response)))
	      (run-hook-with-args 'se-inf-response-hook json))
	  (run-hook-with-args 'se-inf-response-hook response)))
    (error
     (message "%s" (error-message-string err)))))

(defun se-inf-parse-file (&rest file)
  "Sends parse request to current process.  Sends the default
request unless `se-inf-parse-hook' is non-nil.  Uses the current
buffer's file unless FILE is non-nil."
  (interactive)
  (run-hooks 'se-inf-parse-hook)
  (se-inf-ask (or file (buffer-file-name))))

(defun se-inf-save-if-modified ()
  "Save the buffer only if it is modified."
  (interactive)
  (when (buffer-modified-p)
    (save-buffer)))

(defun se-inf-get-spans (json)
  "Returns spans from default formatted JSON."
  (cdr (assoc 'spans json)))

(defun se-inf-process-spans (json)
  "Creates parse tree from spans found in JSON. Sets the variable
`se-mode-parse-tree'."
  (when (se-inf-get-spans json)
    (setq se-mode-parse-tree
	  (se-create-parse-tree
	   (se-create-spans
	    (se-inf-get-spans json))))))

(defun se-inf-get-error (json)
  "Returns possible error from default formatted JSON."
  (cdr (assoc 'error json)))

(defun se-inf-process-error (json)
  "Displays error message found in JSON."
  (let ((msg (se-inf-get-error json)))
    (when msg
      (message "Error: %s" msg))))

(defun se-inf-get-error-span (json)
  "Returns possible error span from default formatted JSON."
  (let ((info (cdr (assoc 'error-span json))))
    (when info
      (apply #'se-new-span info))))

(defun se-inf-process-error-span (json)
  "Highlights the error span found in JSON."
  (let ((span (se-inf-get-error-span json)))
    (when span
      (se-inf-error-overlay span)
      (se-mode-goto-term span))))

(defun se-inf-remove-overlays (&rest args)
  "Removes all overlays from the current buffer."
  (remove-overlays (point-min) (point-max)))

(defun se-inf-error-overlay (span)
  "Creates an overlay over SPAN to indicate an error."
  (let ((overlay (make-overlay (se-term-start span)
			       (se-term-end span))))
    (overlay-put overlay 'info (se-span-data (se-first-span span)))
    (overlay-put overlay 'face "error")
    (overlay-put overlay 'modification-hooks
		 (list (lambda (overlay &rest args)
			 (overlay-put overlay 'face nil))))))

(defun se-inf-kill-emacs-advice (orig &optional arg)
  "Don't query about killing processes if they have
`se-inf-auto-kill' set to a non-nil value."
  (let ((non-auto-kill-procs
	 (cl-remove-if (lambda (proc) (process-get proc 'se-inf-auto-kill)) (process-list))))
    (cl-letf (((symbol-function 'process-list) (lambda () non-auto-kill-procs)))
      (funcall orig arg))))

(if (fboundp #'advice-add)
    (advice-add #'save-buffers-kill-emacs :around #'se-inf-kill-emacs-advice))

(provide 'se-inf)
