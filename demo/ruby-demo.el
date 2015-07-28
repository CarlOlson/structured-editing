
(load-file "../se.el")
(load-file "../se-helpers.el")
(load-file "../se-mode.el")
(load-file "../se-inf.el")

(load-file "../json.el/json.el")

(defun list-to-spans (list)
  (mapcar (lambda (x) (apply 'se-new-span x))
	  list))

(defconst se-mode-demo-tree
  (let ((json-array-type 'list))
    (se-create-parse-tree
     (list-to-spans (json-read-file "ruby-demo-spans")))))

(defun se-ruby-parse-file ()
  (interactive)
  (let ((json nil)
	(file (buffer-file-name))
	(json-array-type 'list))
    (with-temp-buffer
      (call-process "ruby" nil t nil "../tools/spanize.rb" file)
      (setq json (json-read-from-string (buffer-string))))
    (setq se-mode-parse-tree
    	  (se-create-parse-tree (list-to-spans json)))))

(find-file "example.rb")
(se-mode)
(setq se-mode-parse-tree se-mode-demo-tree)
(define-key se-navigation-mode-map (kbd "c") #'se-ruby-parse-file)
