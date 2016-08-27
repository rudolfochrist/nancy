;;; nancy.asd

(in-package :asdf-user)

(defsystem #:nancy
  :version "0.1.0"
  :author "Sebastian Christ"
  :mailto "rudolfo.christ@gmail.com"
  :license "LLGPL"
  :source-control (:git "git@github.com:rudolfochrist/nancy.git")
  :bug-tracker "https://github.com/rudolfochrist/nancy/issues"
  :depends-on (:ningle
               :lack
               :clack
               :clack-errors
               :lack-middleware-clack-errors)
  :components ((:module "src"
                :components ((:file "package")
                             (:file "http-status")
                             (:file "nancy"))))
  :description "A (very) thin web wrapper"
  :in-order-to ((test-op (test-op :nancy-test))))
