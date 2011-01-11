;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:webweight.symbols)

(defparameter *food-types*
  (let ((table (make-hash-table :test 'equal)))
    (dolist (item '(S E B F V P PA C))
      (setf (gethash (symbol-name item) table) item))
    table))
(defun find-food-type (string)
  (or (gethash string *food-types*)
      (error "Unknown symbol type: ~S" string)))

