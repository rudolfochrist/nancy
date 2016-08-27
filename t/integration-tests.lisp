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

(xpost ("/create")
  (let ((name (params "name"))
        (age (params "age")))
    (status :created)
    (format nil "~A::~A" name age)))

(xget ("/status-204")
  (status 204))

(xget ("/status-keyword-204")
  (status :no-content))

(xget ("/new-endpoint")
  "")

(xget ("/old-endpoint")
  (redirect "/new-endpoint"))

(xget ("/other")
  (redirect "/proxy" :status :see-other))

(defun dot-name (name)
  (format nil "::~A::" name))

(xput ("/putting")
  (dot-name (params "name")))

(xpatch ("/patching")
  (dot-name (params "name")))

(xdelete ("/remove/:id")
  (let ((id (params :id)))
    (if (string= "1" id)
        (status :no-content)
        (status :not-found))))

(xoptions ("/show-options")
  (format nil "GET POST PUT PATCH DELETE OPTIONS"))


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

(test custom-status
  (is-response-status 204 (http-request (url "/status-204"))))

(test custom-keyword-status
  (is-response-status 204 (http-request (url "/status-keyword-204"))))

(test post
  (is-response (http-request (url "/create")
                             :parameters '(("name" . "Albert") ("age" . "65"))
                             :method :post)
               :status 201
               :path "/create"
               :body "Albert::65"))

(test put
  (is-response (http-request (url "/putting")
                             :method :put
                             :parameters '(("name" . "Albert")))
               :status 200
               :path "/putting"
               :body "::Albert::"))

(test patch
  (is-response (http-request (url "/patching")
                             :method :patch
                             :parameters '(("name" . "Albert")))
               :status 200
               :path "/patching"
               :body "::Albert::"))

(test delete
  (is-response-status 204 (http-request (url "/remove/1") :method :delete))
  (is-response-status 404 (http-request (url "/remove/42") :method :delete)))

(test options
  (is-response (http-request (url "/show-options") :method :options)
               :status 200
               :path "/show-options"
               :body "GET POST PUT PATCH DELETE OPTIONS"))

(test redirect
  (is-response (http-request (url "/old-endpoint"))
               :status 200
               :path "/new-endpoint"
               :body ""))

(test redirect-keyword
  (is-response (http-request (url "/other") :redirect nil)
               :status 303
               :path "/other"
               :body "/proxy"))

;;; test static files


(defun run-tests ()
  "Runs the integration tests"
  (start :port *port*)
  (5am:run!)
  (stop))

