
(defmacro se-create-mode (prefix parent &optional docstring &rest body)
  "A macro for defining structured editing based modes.  This
macro should contain best practices at the cost of customization.

PREFIX should be the mode's name, properly capitalized.  PREFIX
in lowercase is used as a namespace.  The created mode can be
activated by PREFIX-mode, all lower case.
PARENT should be either nil or a mode to derive the new mode
from.
DOCSTRING is an optional string to document your mode.  It is
highly recommend you do not leave this out.
BODY should contain any code to be executed when the mode starts.
It is expected that `se-inf-start' is called.

Example:
  (se-create-mode \"se-Ruby\" ruby-mode
    \"A structured editing based mode for Ruby files.\"
    (se-inf-start ...))
  => se-ruby-mode"
  (declare (indent 2) (debug t))
  (cl-macrolet ((idf (&rest args)
		     `(intern (downcase (format ,@args)))))
    `(progn
       (define-derived-mode
	 ,(idf "%s-mode" prefix)
	 ,(or parent
	      ;; keep emacs version <24 compatability
	      (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))
	 ,(format "%s" prefix)
	 ,docstring
	 ,@body
	 (define-key ,(idf "%s-mode-map" prefix) (kbd "M-s") #'se-navigation-mode)
	 (add-hook 'se-navigation-mode-hook
		   ',(idf "%s-parse-file" prefix) nil t))
       
       (defun ,(idf "%s-parse-file" prefix) ()
	 "Only parses when navigation mode is active.  This
prevents the navigation mode hook from calling
`se-inf-parse-file' when deactivating.  Most often one should use
`se-inf-parse-file' instead."
	 (when se-navigation-mode
	   (se-inf-parse-file))))))

(provide 'se-macros)
