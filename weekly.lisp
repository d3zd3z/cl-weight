;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Weekly processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:webweight.weekly)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Testing for various diet plans.

(defgeneric meets-minimums (plan events)
  (:documentation "Returns a true value if the events listed meet the
daily minimum requrements for the given plan."))

(defgeneric in-box (plan events)
  (:documentation "Do all of the given events fit in the box for the
given plan?"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rounded-printer (number)
  "Print out number, rounded to an integer."
  (princ-to-string (round number)))

(defun decimal-printer (digits)
  "Create a decimal printer of the given number of digits."
  (let ((format-code (format nil "~~,~AF" digits)))
    (lambda (number)
      (format nil format-code number))))

(defun checkbox-printer (bool)
  ;; TODO: Use something better for a webpage.
  (if bool "X" " "))

(defparameter *summary-columns*
  `((:S ,#'rounded-printer)
    (:C ,#'rounded-printer)
    (:E ,#'rounded-printer)
    (:B ,#'rounded-printer)
    (:F ,#'rounded-printer)
    (:V ,#'rounded-printer)
    (:PA ,#'rounded-printer))
  "The statistic columns that are based on a summary of other
  columns.")

(defparameter *total-columns*
  `((total-calories ,#'rounded-printer)
    (net-calories ,#'rounded-printer)
    (daily-weight ,(decimal-printer 2))
    (total-weight ,(decimal-printer 2))
    (box ,#'checkbox-printer)
    (mins ,#'checkbox-printer))
  "Statistic columns that are computed from other results.")

(defparameter *all-columns*
  `((day ,#'identity)
    (date ,#'identity)
    ,@*summary-columns*
    ,@*total-columns*))

(defmacro define-stat-class (name &optional other-slots)
  `(defclass ,name ()
     (,@(iter (for (slot) in *summary-columns*)
	      (collect `(,slot :initform 0.0)))
      ,@(iter (for (slot) in *total-columns*)
	      (collect `(,slot :initform 0.0)))
	,@other-slots)))

(define-stat-class daily-stats
    ((journal :type journal :initarg :journal)
     (events :type list :initarg :events)
     (day :type (or null string))
     (date :type (or null string))))

(defun daily-numbers (stats)
  "Given a DAILY-STATS, extract all of the columns appropriately
printable."
  (iter (for (slot printer) in *all-columns*)
	(collect (funcall printer (slot-value stats slot)))))

(defun daily-column-names ()
  "Return the names of the columns for the daily status."
  (iter (for (slot) in (append *summary-columns* *total-columns*))
	(collect (string-capitalize (symbol-name slot)))))

(defun update-status (stats event)
  "Update the daily statistics with the given event.  Updates the
summary column for this event type, and updates total and net calories
appropriately.  Weights are not computed here."
  (let ((kind (event-kind event))
	(count (event-count event))
	(calories (event-calories event)))
    (incf (slot-value stats kind) count)
    (incf (slot-value stats 'net-calories) calories)
    (unless (eq kind :PA)
      (incf (slot-value stats 'total-calories) calories))))

(defun summarize-day (stats journal events)
  "Summarize the events for a single day."
  (mapc (lambda (event)
	  (update-status stats event))
	events)
  (setf (slot-value stats 'daily-weight)
	(/ (- (slot-value stats 'net-calories)
	      (* 13.0 (journal-weight journal)))
	   500))
  (values))

(defun adjust-weight-change (statses)
  "Walk through the list of STATSes and update the TOTAL-WEIGHT column
to be a running average."
  (iter (for stats in statses)
	(for count from 1)
	(sum (slot-value stats 'daily-weight) into weight-sum)
	(setf (slot-value stats 'total-weight)
	      (/ weight-sum count))))

(defun set-plan-columns (journal stats events)
  "Update the stats and update the columns having to do with this
particular plan."
  (setf (slot-value stats 'mins)
	(meets-minimums (journal-plan journal) events))
  (setf (slot-value stats 'box)
	(in-box (journal-plan journal) events)))

(defun compute-stats (journal)
  "Given a weekly journal, return a list of the stats."
  (let ((statses (mapcar (lambda (events)
			   (make-instance 'daily-stats
					  :journal journal
					  :events events))
			 (journal-weeks journal))))
    (adjust-weight-change statses)
    (iter (for stats in statses)
	  (for events in (journal-weeks journal))
	  (set-plan-columns journal stats events))
    statses))

(defun compute-weekly (journal)
  "Returns the information on the weekly status chart."
  (values (daily-column-names)
	  (mapcar #'daily-numbers (compute-stats journal))))

(defmethod shared-initialize :after ((instance daily-stats)
				     slots &rest initargs
				     &key journal events)
  (declare (ignore slots initargs))
  (assert journal (journal) "Must specify a :journal")
  (assert events (events) "Must specify events")
  (let ((first-event (first events)))
    (assert first-event (first-event) "Must be at least one event in a given day")
    (summarize-day instance journal events)
    (setf (slot-value instance 'day) (date-to-dow (event-date first-event)))
    (setf (slot-value instance 'date) (date-to-short (event-date first-event)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The moderate diet plan requires each day to have at least 5 70+
;;; shakes, and at least 2 of a certain list of entrees.

(defparameter *moderate-entrees*
  '("bbq" "5bean" "chili" "pasta-fagioli" "pot-roast")
  "Entrees that meet the minimum requirements for the moderate
  program.")

(defun moderate-shake-p (event)
  "Does this event qualify for the shake minimum?"
  (and (eq (event-kind event) :S)
       (string= (event-name event) "70+")))

(defun moderate-entree-p (event)
  "Does this event qualify for the entree minimum?"
  (and (eq (event-kind event) :E)
       (member (event-name event) *moderate-entrees* :test #'string=)))

(defmethod meets-minimums ((plan (eql :moderate)) events)
  (iter (for event in events)
	(counting (moderate-shake-p event) into shakes)
	(counting (moderate-entree-p event) into entrees)
	(finally (return (and (>= shakes 5) (>= entrees 2))))))

(defmethod in-box ((plan (eql :moderate)) events)
  (iter (for event in events)
	(always (member (event-kind event)
			'(:S :E :B :PA)))))
