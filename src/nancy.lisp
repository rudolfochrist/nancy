;;; nancy.lisp

(in-package :cl-user)
(defpackage #:nancy
  (:use :cl)
  (:import-from :lack.response
                #:response-status
                #:response-headers)
  (:import-from :lack
                #:builder)
  (:import-from :ningle
                *request*
                *response*) 
  (:export
   #:*webapp*
   #:*request*
   #:*response*
   #:xget
   #:xpost
   #:xput
   #:xpatch
   #:xdelete
   #:xoptions
   #:redirect
   #:*webapp-config*
   #:stop
   #:start
   #:params))

(in-package :nancy)

(defvar *webapp* (make-instance 'ningle:<app>)
  "Ningle web app instance")

(defmacro http-handler-skeleton (http-method)
  "HTTP hanlder skeleton for different HTTP-METHODs.
This macro should expand in an environment with bound URL and BODY variables."
  (let ((g!params (gensym "params")))
    ``(setf (ningle:route nancy:*webapp*
                          ,url
                          ,@(if regexp (list :regexp t))
                          :method ,,http-method)
            (lambda (,',g!params)
              (declare (ignorable ,',g!params))
              (flet ((params (&optional name)
                       (if name
                           (cdr (assoc name ,',g!params :test #'string-equal))
                           ,',g!params)))
                ,@body)))))

(defmacro xget ((url &key regexp) &body body)
  "Create handler for GET requests."
  (declare (ignorable regexp))
  (http-handler-skeleton :get))

(defmacro xpost ((url &key regexp) &body body)
  "Create handler for POST requests."
  (declare (ignorable regexp))
  (http-handler-skeleton :post))

(defmacro xput ((url &key regexp) &body body)
  "Create handler for PUT requests."
  (declare (ignorable regexp))
  (http-handler-skeleton :put))

(defmacro xpatch ((url &key regexp) &body body)
  "Create handler for PATCH requests."
  (declare (ignorable regexp))
  (http-handler-skeleton :patch))

(defmacro xdelete ((url &key regexp) &body body)
  "Create handler for DELETE requests."
  (declare (ignorable regexp))
  (http-handler-skeleton :delete))

(defmacro xoptions ((url &key regexp) &body body)
  "Create handler for OPTIONS requests."
  (declare (ignorable regexp))
  (http-handler-skeleton :options))

(defun redirect (url &key (status 302))
  "Redirect to URL with STATUS."
  (let ((res *response*))
    (setf (response-status res) status
          (getf (response-headers res) :location) url)
    url))


;;; Configuration

(defvar *static-root* #p"/static/"
        "Static file location.")

(defvar *static-path* "/public/"
  "URL path for static files.")


;;; Server Management

(defvar *handler* nil
  "Server handler.")

(defun stop ()
  "Stops the server and halts the web application"
  (when *handler*
    (prog1
        (clack:stop *handler*)
      (setf *handler* nil))))

(defun start (&key (server :hunchentoot) (port 4242) productionp)
  "Starts the server SERVER on port PORT.

Set :PRODUCTIONP to T if you run this on your production
server. Currently PRODUCTIONP has only effect on SBCL."
  #-sbcl (declare (ignore productionp))
  (when *handler*
    (restart-case (error "Server already running.")
      (restart-server ()
        :report "Restart the server"
        (stop))))
  (setf *handler*
        (clack:clackup (builder
                        (:static :root *static-root*
                                 :path *static-path*)
                        #+sbcl
                        (if productionp
                            nil
                            :clack-errors)
                        *webapp*) 
                       :server server
                       :port port)))



