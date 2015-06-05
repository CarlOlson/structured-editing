
(eval-when-compile (require 'cl))

(defmacro curry (f &rest args)
  `(lambda (&rest more-args)
     (apply ',f ,@args more-args)))
