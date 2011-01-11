;;; CL-weight package

(defpackage #:webweight.symbols
  (:use #:cl)
  (:export #:S #:E #:B #:F #:V #:P #:PA #:C
	   #:find-food-type))

(defpackage #:webweight.foods
  (:use #:cl #:csv-parser #:webweight.symbols)
  (:export #:load-foods #:lookup-food))

(defpackage #:webweight.journal
  (:use #:cl #:iterate #:webweight.symbols #:webweight.foods)
  (:export #:parse-journal
	   #:all-journal-names #:load-journal
	   #:event #:eat-event #:exercise-event
	   #:event-calories #:event-name
	   #:event-date #:event-kind #:event-count
	   #:date-to-dow #:date-to-short
	   #:journal #:journal-p #:journal-plan #:journal-weight #:journal-weeks))

(defpackage #:webweight.weekly
  (:use #:cl #:iterate #:webweight.journal #:webweight.symbols)
  (:export #:all-journal-names
	   #:load-journal
	   #:compute-weekly))

(defpackage #:webweight.web
  (:use #:cl #:iterate #:hunchentoot #:cl-who
	#:webweight.weekly)
  (:export #:start-webweight #:stop-webweight))

(defpackage #:webweight
  (:use #:cl #:iterate
	#:webweight.journal
	#:webweight.weekly
	#:webweight.web)
  (:export #:start-webweight #:stop-webweight))

