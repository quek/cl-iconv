;;;; -*- lisp -*-
(defsystem iconv
  :name "iconv"
  :author "Yoshinori Tahara <read.eval.print@gmail.com>"
  :version "0.3"
  :components ((:file "iconv"))
  :depends-on (cffi))
