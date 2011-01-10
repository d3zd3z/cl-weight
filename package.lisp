;;; CL-weight package

(defpackage #:webweight.foods
  (:use #:cl #:csv-parser)
  (:export #:load-foods #:lookup-food))

(defpackage #:webweight.journal
  (:use #:cl #:iterate #:webweight.foods)
  (:export #:parse-journal
	   #:all-journal-names #:load-journal
	   #:event #:eat-event #:exercise-event
	   #:event-calories #:event-name
	   #:event-date #:event-kind #:event-count
	   #:date-to-dow #:date-to-short
	   #:journal #:journal-p #:journal-plan #:journal-weight #:journal-weeks))

(defpackage #:webweight.weekly
  (:use #:cl #:iterate #:webweight.journal))

(defpackage #:webweight
  (:use #:cl #:iterate
	#:webweight.journal
	#:webweight.weekly))

