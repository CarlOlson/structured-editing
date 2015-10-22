
(eval-when-compile (require 'cl))

(make-variable-buffer-local
 (defvar se-highlight-font-map nil
   "Should be a mapping of faces or an abreviated font-lock
face (function-name, variable-name, keyword, comment, type,
constant, string, etc) as symbols. The cdr should be a list of
se-span names to give that face."))

(defun se-highlight ()
  (interactive)
  (when se-highlight-font-map
    (let ((modified (buffer-modified-p)))
      (font-lock-mode -1)
      (se-mapc #'se-highlight-term se-mode-parse-tree)
      (set-buffer-modified-p modified))))

(defun se-highlight-term (TERM)
  (let ((name (se-term-name TERM))
	(start (se-term-start TERM))
	(end (se-term-end TERM))
	(face
	 (loop for (face . names) in se-highlight-font-map
	       when (member (se-span-name TERM) names)
	       do (return face))))
    (when face
      (put-text-property start end 'face (se-highlight-to-face face) nil))))

(defun se-highlight-to-face (FACE)
  "Returns font lock face symbol abbreviated by FACE if exists,
otherwise returns FACE."
  (let ((orig FACE))
    (when (symbolp FACE)
      (setq FACE (symbol-name FACE)))
    (if (not (stringp FACE))
	orig
      (setq FACE (intern (concat "font-lock-" FACE "-face")))
      (if (and (boundp FACE)
	       (symbol-value FACE))
	  FACE orig))))

(provide 'se-highlight)
