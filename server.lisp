;;; Web server

(in-package #:webweight.web)

;;; Set the XML header in the prolog
(setf *prologue*
      "<?xml version=\"1.0\" encoding=\"utf-8\" ?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")
(setf *default-content-type* "text/html; charset=utf-8")

(defvar *webweight-acceptor* nil)

(defun start-webweight (&optional (port 4242))
  (when *webweight-acceptor*
    (error "Webweight already started"))
  (setf *webweight-acceptor*
	(hunchentoot:start (make-instance 'hunchentoot:acceptor :port port))))
(defun stop-webweight ()
  (unless *webweight-acceptor*
    (error "Webweight not running"))
  (hunchentoot:stop *webweight-acceptor*)
  (setf *webweight-acceptor* nil))

(setf *dispatch-table*
      (list 'dispatch-easy-handlers
	    (create-folder-dispatcher-and-handler
	     "/css/"
	     #p"/home/davidb/web/cl-weight/static/css/" "text/css")
	    (create-folder-dispatcher-and-handler
	     "/images/"
	     #p"/home/davidb/web/cl-weight/static/images/")
	    (create-regex-dispatcher "^/wlog/weekly/\\d{4}-\\d\\d-\\d\\d" 'weekly)
	    (create-prefix-dispatcher "/wlog/weekly" 'current-weekly)
	    (create-prefix-dispatcher "/wlog" 'wlog)
	    'default-dispatcher))

(setf *default-handler* 'index)


(defun gen-head (out title)
  (flet ((stylesheet (name &optional (media "screen, projection"))
	   (with-html-output (out out)
	     (:link :rel "stylesheet"
		    :href (concatenate 'string "/css/" name)
		    :type "text/css"
		    :media media))))
    (with-html-output (out out)
      (:head
       (:title (esc title))
       (:link :href "/images/broccoli.ico" :rel "SHORTCUT ICON")
       (stylesheet "blueprint/screen.css")
       (stylesheet "blueprint/print.css" "print")
       (stylesheet "blueprint/fancy-type.css")
       "<!-- [if lt IE 8]>"
       (stylesheet "blueprint/ie.css")
       "![endif]-->"
       (stylesheet "sidebar.css" "screen, projection, print")
       (stylesheet "print.css" "print")))))

(defmacro html-template (out title &body body)
  "Generate the standard template with the given page TITLE, and the
  BODY in the appropriate container."
  (let ((the-title (gensym "TITLE-")))
    `(let ((,the-title ,title))
       (with-html-output-to-string (,out nil :prologue t :indent nil)
	 ((:html :xmlns "http://www.w3.org/1999/xhtml"
		 :|xml:lang| "en"
		 :lang "en")
	  (gen-head out ,the-title)
	  (:body
	   ((:div :class "container")
	    ((:div :class "column span-24 last"
		   :style "text-algin: center")
	     ((:h1 :class "alt")
	      (esc ,the-title)))
	    (:hr)
	    ((:div :class "column span-4 colborder sidebar")
	     (:hr :class "space")
	     (:ul
	      (:li (:a :href "/" "Home"))
	      (:li (:a :href "/wlog" "Webweight"))
	      (:li (:a :href "/wlog/weekly" "Summary sheet")))
	     (:hr :class "space"))
	    ((:div :class "column span-19 last")
	     ,@body))))))))

(defun index ()
  (html-template out "Homepage of David Brown"
    (:h1 "Blog")
    (:p "I now "
	(:a :href "http://blog.davidb.org" "have a blog")
	". Most of my new information will be there rather than on this site.")))

(defun wlog ()
  "Weight log"
  (html-template out "David Brown summary sheets"
    ((:div :class "column span-8")
     ((:ul :class "wlog")
      (iter (for date in (all-journal-names))
	    (htm (:li
		  ((:a :href (format nil "/wlog/weekly/~A" date))
		      (str date)))))))))

(defun make-weekly (week)
  (let ((journal (load-journal week)))
    (multiple-value-bind (heading body-rows summary)
	(compute-weekly journal)
      (flet ((output-heading (out)
	       (with-html-output (out out)
		 (:tr
		  (iter (for head in heading)
			(htm (:th (esc head)))))))
	     (output-row (out row)
	       (with-html-output (out out)
		 (:tr
		  (iter (for col in row)
			(htm (:td (esc col))))))))
	(html-template out "Weekly summary"
	  (:h1 "Weekly summary sheet")
	  ;; TODO: Use last date, not first.
	  (:h2 (esc week))
	  ((:table :class "weekly")
	   (:tbody
	    (output-heading out)
	    (iter (for row in body-rows)
		  (output-row out row))
	    (output-heading out)
	    (output-row out summary))))))))

(defun weekly ()
  (ppcre:register-groups-bind (url)
      ("^/wlog/weekly/(.*)" (script-name*))
    (make-weekly url)))

(defun current-weekly ()
  (make-weekly (first (all-journal-names))))

;;; TODO: Default handler should be for errors, homepage handles it correctly.
