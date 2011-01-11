;;; Webweight descriptor

(defsystem :webweight
  :serial t
  :version "0.1"
  :depends-on (:cl-ppcre
	       :csv-parser
	       :date-calc   ;; WARNING GPLv2
	       :parse-number
	       :cl-who
	       :trivial-garbage
	       :bordeaux-threads
	       :hunchentoot)
  :components ((:file "package")
	       (:file "symbols")
	       (:file "foods")
	       (:file "journal")
	       (:file "weekly")
	       (:file "server")))
