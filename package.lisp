;;; CL-weight package

(defpackage #:webweight.foods
  (:use #:cl #:csv-parser)
  (:export #:load-foods #:lookup-food))

(defpackage #:webweight.journal
  (:use #:cl #:iterate #:webweight.foods)
  (:export #:parse-journal
	   #:all-journal-names #:load-journal
	   #:journal #:journal-p #:journal-plan #:journal-weight #:journal-weeks))

(defpackage #:webweight.weekly
  (:use #:cl #:iterate #:webweight.journal))

(defpackage #:webweight
  (:use #:cl #:iterate
	#:webweight.journal
	#:webweight.weekly))

