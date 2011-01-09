;;; Web server

(in-package #:webweight)

(hunchentoot:start (make-instance 'hunchentoot:acceptor :port 4242))

(hunchentoot:define-easy-handler (say-yo :uri "/yo") (name)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "Hey~@[ ~A~]!" name))
