;;; nancy-test.asd

(in-package :asdf-user)

(defsystem #:nancy-test
  :author "Sebastian Christ"
  :mailto "rudolfo.christ@gmail.com"
  :description "Test system of nancy"
  :license "LLGPL"
  :depends-on (:fiveam
               :drakma
               :nancy
               :puri)
  :components ((:module "t"
                :components ((:file "package")
                             (:file "integration-tests"))))
  :perform (test-op (op c)
                    (asdf:clear-system c)
                    (uiop:symbol-call :nancy-test :run-tests)))
