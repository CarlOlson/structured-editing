
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

(load-file "../se-mode.el")

(find-file "example.rb")
(se-mode)
(setq se-mode-parse-tree se-mode-demo-tree)
(define-key se-navigation-mode-map (kbd "c") #'se-mode-parse-file)

