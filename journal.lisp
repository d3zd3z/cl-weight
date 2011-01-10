;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Web weight journal processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:webweight.journal)

(defparameter *matchers* nil)

;;; A given line of input can possibly be transformed into a sexp.
;;; Each group can have a possible conversion function applied (or nil
;;; to indicate leave as a string.
;;; Defining a pattern associates the pattern handler with the PATTERN
;;; property on the name symbol.

(defun make-keyword (string)
  (values (intern (string-upcase string) "KEYWORD")))

(defun convert-line (func regex mapping line)
  "Try matching the line against the regex.  If it matches, use the
list of mapping functions on the group results.  A map can be a
function, which will be called.  T indicates the identity, and NIL
means this group argument should not be converted.

Returns true if the mach was successful, or NIL if the line didn't
match."
  (multiple-value-bind (matched groups)
      (ppcre:scan-to-strings regex line)
    (when matched
      (apply func
	     (iter (for mapper in mapping)
		   (for arg in-vector groups)
		   (cond ((functionp mapper)
			  (collect (funcall mapper arg)))
			 ((eq mapper t)
			  (collect arg))
			 ((null mapper)))))
      t)))

(defmacro define-pattern (name regex mapping)
  (let ((nmapping (mapcar (lambda (item)
			    (cond ((or (null item) (eq item t))
				   item)
				  ((symbolp item)
				   `#',item)
				  (t (error "Invalid mapping in pattern"))))
			  mapping))
	(line (gensym))
	(compiled-regex (gensym)))
    `(let ((,compiled-regex (ppcre:create-scanner ,regex)))
       (setf (get ',name 'pattern)
	     (lambda (,line)
	       (convert-line #',name ,compiled-regex (list ,@nmapping) ,line))))))

;;; Special placeholders for constructed values.
(defvar *foods*)
(defvar *weight*)
(defvar *plan*)
(defvar *this-date*)
(defvar *today-list*)
(defvar *week-list*)

(defun test-setup ()
  (setf *foods* (load-foods)
	*today-list* nil))

(defstruct date year month day hour minute)
(defun add-time (date hour minute)
  "Create a new DATE based off of the day in DATE and the hour/minute
given."
  (make-date :year (date-year date)
	     :month (date-month date)
	     :day (date-day date)
	     :hour hour
	     :minute minute))

(defun date-to-dow (date)
  "Return the the character abbreviation for the day of week for the
given date."
  (aref #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
	(date-calc:cl-day-of-week (date-year date)
				  (date-month date)
				  (date-day date))))

(defun date-to-short (date)
  "Return a short string for the given date."
  (format nil "~2,'0d-~2,'0d"
	  (date-month date)
	  (date-day date)))

(defclass event ()
  ((date :type date :accessor event-date)
   (kind :type symbol :initarg :kind :accessor event-kind)
   (count :type number :initarg :count :accessor event-count)
   (name :type (or null string) :initarg :name :accessor event-name
	 :initform nil)))

(defmethod shared-initialize :after ((instance event) slots
				     &rest initargs
				     &key hour minute)
  (declare (ignore slots initargs))
  (setf (slot-value instance 'date)
	(add-time *this-date* hour minute)))

(defgeneric event-calories (event))

(defclass eat-event (event)
  ((calories :type number)))

(defmethod shared-initialize :after ((instance eat-event) slots
				     &rest initargs
				     &key count kind name)
  (declare (ignore slots initargs))
  (setf (slot-value instance 'calories)
	(* count (lookup-food kind name *foods*))))

(defclass exercise-event (event)
  ((calories :type number :initarg :calories)
   (name :type string :initarg :name)))

(defmethod event-calories ((event eat-event))
  (* (slot-value event 'calories)
     (event-count event)))
(defmethod event-calories ((event exercise-event))
  (* (- (slot-value event 'calories))
     (event-count event)))

(defun set-weight (weight)
  (setf *weight* weight))
(define-pattern set-weight "^weight\\s*=\\s*([\\d\\.]+)$"
  (read-from-string))

(defun ignore-comment ())
(define-pattern ignore-comment "^\\s*#.*$" ())

(defun ignore-blank-line ())
(define-pattern ignore-blank-line "^\\s*$" ())

(defun set-plan (plan)
  (setf *plan* plan))
(define-pattern set-plan "^plan\\s*=\\s*([a-z-]+)$"
  (make-keyword))

(defun set-date (year month day)
  (when *this-date*
    (push (reverse *today-list*)
	  *week-list*)
    (setf *today-list* nil))
  (setf *this-date* (make-date :year year :month month :day day)))
(define-pattern set-date "^(\\d{4})-(\\d\\d)-(\\d\\d):$"
  (parse-integer parse-integer parse-integer))

(defun add-exercise (hour minute count name)
  (push (make-instance 'exercise-event :kind :PA
		       :hour hour
		       :minute minute
		       :calories 1.0
		       :count count
		       :name name)
	*today-list*))
(define-pattern add-exercise "^\\s+(\\d\\d):(\\d\\d) PA (\\d+) (.*)$"
  (parse-integer parse-integer parse-integer t))

(defun add-counted (hour minute kind count name)
  (push (make-instance 'eat-event
		       :hour hour
		       :minute minute
		       :kind kind
		       :name name
		       :count count)
	*today-list*))
(defun add-uncounted (hour minute kind name)
  (add-counted hour minute kind 1.0 name))
(define-pattern add-uncounted "^\\s+(\\d\\d):(\\d\\d) (S|E|B) (.*)$"
  (parse-integer parse-integer make-keyword t))

(define-pattern add-counted "^\\s+(\\d\\d):(\\d\\d) (F|V|P) ([\\d\\.]+) (.*)$"
  (parse-integer parse-integer make-keyword read-from-string t))

(defun add-cereal (hour minute)
  (add-uncounted hour minute :C "cereal"))
(define-pattern add-cereal "^\\s+(\\d\\d):(\\d\\d) C$"
  (parse-integer parse-integer))

(defparameter *patterns*
  (mapcar (lambda (name)
	    (get name 'pattern))
	  '(ignore-comment
	    ignore-blank-line
	    set-weight
	    set-plan
	    set-date
	    add-exercise
	    add-uncounted
	    add-counted
	    add-cereal)))

(defstruct journal
  plan weight
  weeks)

(defun parse-journal (path)
  (let ((*foods* (load-foods))
	(*this-date*)
	(*today-list*)
	(*weight*)
	(*plan*)
	(*week-list*))
    (iter (for line in-file path using #'read-line)
	  (iter (for matcher in *patterns*)
		(thereis (funcall matcher line))
		(finally (error (format nil "Invalid input line: ~A" line)))))
    (set-date nil nil nil)
    (make-journal :plan *plan*
		  :weight *weight*
		  :weeks (reverse *week-list*))))

(defparameter *journal-dirs*
  '(#p"/home/davidb/weight/"
    #p"/home/davidb/weight/old/"))

(defun all-journal-names ()
  "Scan the journal directories, and return a sorted list of the
possible journal names."
  (iter outer
	(for dir in *journal-dirs*)
	(iter (for pathname in (directory (make-pathname :name :wild
							 :type "dat"
							 :defaults dir)))
	      (for name = (pathname-name pathname))
	      (when (ppcre:scan "^\\d{4}-\\d\\d-\\d\\d$" name)
		(in outer (collect name into result))))
	(finally (return-from outer (sort result #'string>)))))

(defun find-journal (name)
  "Return the full pathname of the specified journal file, or NIL if
it couldn't be found."
  (iter (for dir in *journal-dirs*)
	(for pathname = (directory (make-pathname :name name
						  :type "dat"
						  :defaults dir)))
	(when pathname (return (first pathname)))))

(defun load-journal (name)
  "Load the specified journal."
  (let ((path (find-journal name)))
    (unless path
      (error "Unable to find journal named ~S" name))
    (parse-journal path)))
