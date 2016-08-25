;;; nancy.asd

(in-package :asdf-user)

(defsystem #:nancy
  :version "0.1"
  :author "Sebastian Christ"
  :mailto "rudolfo.christ@gmail.com"
  :license "LLGPL"
  :source-control (:git "git@github.com:rudolfochrist/nancy.git")
  :bug-tracker "https://github.com/rudolfochrist/nancy/issues"
  :depends-on (:ningle
               :lack
               :clack
               :clack-errors
               :lack-middleware-errors)
  :components ((:module "src"
                :components ((:file "nancy"))))
  :description "A (very) thin web wrapper"
  :in-order-to ((test-op (test-op :nancy-test))))
