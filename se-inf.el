
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

(defun se-inf-start ()
  "Expects `se-inf-process' to have already been set. Should be
called at the start of an `se-mode'."
  (setq se-inf-queue (tq-create se-inf-process)))

(defun se-inf-stop ()
  "Should be called at the end of an `se-mode'."
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

(defun se-inf-process-json-string (str)
  (let* ((json-array-type 'list)
	 (json (json-read-from-string str)))
    (se-inf-process-error (se-inf-get-error json) json)
    (se-inf-process-spans (se-inf-get-spans json) json)
    json))
  
