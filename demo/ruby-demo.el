
(load-file "../se-helpers.el")
(load-file "../se.el")

(defconst json-array-type 'list)
(load-file "../json.el/json.el")

(defun list-to-spans (list)
  (mapcar (lambda (x) (apply 'se-new-span x))
	  list))

(defconst se-mode-demo-tree
  (se-create-parse-tree
   (list-to-spans (json-read-file "ruby-demo-spans"))))

(defun se-mode-parse-file ()
  (interactive)
  (let ((json nil)
	(file (buffer-file-name)))
    (with-temp-buffer
      (call-process "ruby" nil t nil "../tools/spanize.rb" file)
      (message (buffer-string))
      (setq json (json-read-from-string (buffer-string))))
    (setq se-mode-parse-tree
    	  (se-create-parse-tree (list-to-spans json)))))

(defun se-mode-inspect ()
  (interactive)
  (when (se-mode-selected)
    (let* ((msg nil)
	   (span (se-node-parent (se-mode-selected)))
	   (data (se-span-data span)))
      (cond
       ((null data)
	(setq msg "No node data."))
       (:else
	(setq msg data)))
      (se-mode-popup-window "*se*"
			    (concat msg (format "\n\nType:\t%s\nStart:\t%d\nEnd:\t%s"
						(se-span-type span)
						(se-span-start span)
						(se-span-end span)))))))

(load-file "../se-mode.el")

(find-file "example.rb")
(se-mode)
(setq se-mode-parse-tree se-mode-demo-tree)
(define-key se-mode-map (kbd "C-c c") #'se-mode-parse-file)
(define-key se-mode-map (kbd "C-c i") #'se-mode-inspect)
