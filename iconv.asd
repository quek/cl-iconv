;;;; -*- lisp -*-
(defsystem iconv
  :defsystem-depends-on ("cffi-grovel")
  :depends-on ("cffi")
  :name "iconv"
  :author "Yoshinori Tahara <read.eval.print@gmail.com>"
  :version "0.3"
  :serial t
  :components ((:file "package")
               (:cffi-grovel-file "grovell")
               (:file "iconv")))
