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

;;; If there is no plan specified, these are always false.
;;; TODO: Probably best to assume this is maintenance.
(defmethod meets-minimums ((plans t) events)
  (declare (ignore events))
  nil)

(defmethod in-box ((plans t) events)
  (declare (ignore events))
  nil)

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
  (if bool (make-string 1 :initial-element (code-char #x2714)) " "))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter *summary-columns*
    `((S ,#'rounded-printer)
      (C ,#'rounded-printer)
      (E ,#'rounded-printer)
      (B ,#'rounded-printer)
      (F ,#'rounded-printer)
      (V ,#'rounded-printer)
      (PA ,#'rounded-printer))
    "The statistic columns that are based on a summary of other
  columns.")

  (defparameter *total-columns*
    `((total-cals ,#'rounded-printer)
      (net-cals ,#'rounded-printer)
      (daily-weight ,(decimal-printer 2))
      (total-weight ,(decimal-printer 2))
      (box ,#'checkbox-printer)
      (mins ,#'checkbox-printer))
    "Statistic columns that are computed from other results.")

  (defparameter *all-columns*
    `((day ,#'identity)
      (date ,#'identity)
      ,@*summary-columns*
      ,@*total-columns*)))

(defmacro define-stat-class (name &optional other-slots)
  `(defclass ,name ()
     (,@(iter (for (slot) in *summary-columns*)
	      (collect `(,slot :initform 0.0)))
      ,@(iter (for (slot) in *total-columns*)
	      (collect `(,slot :initform 0.0)))
	,@other-slots)))

(define-stat-class base-stats
    ((journal :type journal :initarg :journal)
     (events :type list :initarg :events)
     (day :type (or null string))
     (date :type (or null string))
     (P :initform 0.0)))

(defclass daily-stats (base-stats) ())
(defclass summary-stats (base-stats) ())

(defun daily-numbers (stats)
  "Given a DAILY-STATS, extract all of the columns appropriately
printable."
  (iter (for (slot printer) in *all-columns*)
	(collect (funcall printer (slot-value stats slot)))))

(defun daily-column-names ()
  "Return the names of the columns for the daily status."
  (iter (for (slot) in *all-columns*)
	(collect (string-capitalize (symbol-name slot)))))

(defun update-status (stats event)
  "Update the daily statistics with the given event.  Updates the
summary column for this event type, and updates total and net calories
appropriately.  Weights are not computed here."
  (let ((kind (event-kind event))
	(count (event-count event))
	(calories (event-calories event)))
    (incf (slot-value stats kind) count)
    (incf (slot-value stats 'net-cals) calories)
    (unless (eq kind 'PA)
      (incf (slot-value stats 'total-cals) calories))))

(defun summarize-day (stats journal events)
  "Summarize the events for a single day."
  (mapc (lambda (event)
	  (update-status stats event))
	events)
  (setf (slot-value stats 'daily-weight)
	(/ (- (slot-value stats 'net-cals)
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

(defun compute-summary (week-stats)
  (let ((summary (make-instance 'summary-stats)))
    (setf (slot-value summary 'day) "")
    (setf (slot-value summary 'date) "")
    ;; TODO: This is kind of hacky.
    (iter (for day in week-stats)
	  (counting (slot-value day 'mins) into mins-count)
	  (counting (slot-value day 'box) into box-count)
	  (iter (for slot in '(S C E B F V PA total-cals net-cals))
		(incf (slot-value summary slot)
		      (slot-value day slot)))
	  (let ((weight (slot-value day 'total-weight)))
	    (iter (for slot in '(daily-weight total-weight))
		  (setf (slot-value summary slot) weight)))
	  (finally
	   (return (let ((numbers (subseq (daily-numbers summary) 0 13))
			 (day-count (length week-stats)))
		     `(,@numbers
		       ,(format nil "~a/~a" box-count day-count)
		       ,(format nil "~a/~a" mins-count day-count))))))))

(defun compute-weekly (journal)
  "Returns the information on the weekly status chart."
  (let ((week-stats (compute-stats journal)))
    (values (daily-column-names)
	    (mapcar #'daily-numbers week-stats)
	    (compute-summary week-stats))))

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
  (and (eq (event-kind event) 'S)
       (string= (event-name event) "70+")))

(defun moderate-entree-p (event)
  "Does this event qualify for the entree minimum?"
  (and (eq (event-kind event) 'E)
       (member (event-name event) *moderate-entrees* :test #'string=)))

(defmethod meets-minimums ((plan (eql :moderate)) events)
  (iter (for event in events)
	;; TODO: Use the count.  Although shakes and entrees always
	;; have a count of 1, other things will have other counts.
	(counting (moderate-shake-p event) into shakes)
	(counting (moderate-entree-p event) into entrees)
	(finally (return (and (>= shakes 5) (>= entrees 2))))))

(defmethod in-box ((plan (eql :moderate)) events)
  (iter (for event in events)
	(always (member (event-kind event)
			'(S E B PA)))))
