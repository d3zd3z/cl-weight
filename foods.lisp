;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:webweight.foods)

(defparameter *food-path* #p"/home/davidb/weight/foods.csv")

(defun load-foods (&optional (path *food-path*))
  (let ((foods (make-hash-table :test 'equal)))
    (do-csv-file ((fields num-fields) path :skip-lines 1)
      (assert (= num-fields 3))
      (setf (gethash (cons (find-food-type (string-upcase (first fields)))
			   (second fields))
		     foods)
	    (parse-number:parse-real-number (third fields))))
    foods))

(defun lookup-food (kind name foods)
  (or (gethash (cons kind name) foods)
      (error (format nil "Unknown food ~S ~S" kind name))))

