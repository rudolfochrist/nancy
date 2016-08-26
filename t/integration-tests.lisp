;;; integration-tests.lisp

(in-package :nancy-test)

(def-suite :nancy-integration
  :description "Integration tests for nancy.")
(in-suite :nancy-integration)


;;; Helpers

(defvar *port* (+ 1024
                  (random 64000)))

(defun url (path)
  (format nil "http://localhost:~D~A" *port* path))

(defmacro is-response-status (status request)
  "Assert if server responded with STATUS for REQUEST."
  `(is (= ,status (nth-value 1 ,request))))

(defmacro is-response (request &key status path body)
  "Assert if server response has desired values."
  (let ((g!resp-body (gensym "resp-body"))
        (g!resp-status (gensym "resp-status"))
        (g!resp-headers (gensym "resp-headers"))
        (g!resp-path (gensym "resp-path")))
    `(multiple-value-bind (,g!resp-body ,g!resp-status ,g!resp-headers ,g!resp-path) ,request
       (declare (ignore ,g!resp-headers))
       (is (= ,g!resp-status ,status))
       (is (string= (puri:uri-path ,g!resp-path) ,path))
       (is (string= ,g!resp-body ,body)))))


;;; Test application

(xget ("/bingo")
  "bingo")

(xget ("/upper-foo")
  (let ((word (params "word")))
    (string-upcase (format nil "foo-~A-foo" word))))

(xget ("/users/:id")
  ;; emulate a user object
  (format nil "#<USER ID=~A>" (params :id)))

(xget ("/error")
  (error "Serious error occurred!"))


;;; Tests

(test get
  (is-response (http-request (url "/bingo"))
               :status 200
               :path "/bingo"
               :body "bingo")
  (is-response (http-request (url "/upper-foo?word=bar"))
               :status 200
               :path "/upper-foo"
               :body "FOO-BAR-FOO")
  (is-response (http-request (url "/upper-foo"))
               :status 200
               :path "/upper-foo"
               :body "FOO-NIL-FOO")
  (is-response (http-request (url "/users/42"))
               :status 200
               :path "/users/42"
               :body "#<USER ID=42>")
  #+sbcl
  (is-response-status 500 (http-request (url "/error"))))

(test home-is-404
  (is-response-status 404 (http-request (url "/") :method :get)))

;;; test post with params

;;; test put with params

;;; test patch with params

;;; test delete

;;; test options

;;; test redirect

;;; test static files


(defun run-tests ()
  "Runs the integration tests"
  (start :port *port*)
  (5am:run!)
  (stop))

