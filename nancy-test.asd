;;; nancy-test.asd

(in-package :asdf-user)

(defsystem #:nancy-test
  :author "Sebastian Christ"
  :mailto "rudolfo.christ@gmail.com"
  :description "Test system of nancy"
  :license "LLGPL"
  :depends-on (:fiveam
               :nancy)
  :components ((:module "t"
                :components ((:file "nancy"))))
  :perform (test-op (op c)
                    (asdf:clear-system c)
                    (uiop:symbol-call :5am :run!)))
