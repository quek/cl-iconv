(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :iconv)
  (asdf:oos 'asdf:load-op :ptester))

(ptester:with-tests ()
  (ptester:test
   (list #(164 162 164 164 164 166) t)
   (iconv:iconv "UTF-8" "EUC-JP" #(227 129 130 227 129 132 227 129 134))
   :test #'equalp
   :multiple-values t)

  (ptester:test
   (list #(0 63 63 164 164 164 166) nil)
   (iconv:iconv "UTF-8" "EUC-JP" #(0 129 130 227 129 132 227 129 134))
   :test #'equalp
   :multiple-values t)

  (ptester:test
   (list #(164 162 164 164 164 166) t)
   (iconv:iconv :utf-8 :euc-jp #(227 129 130 227 129 132 227 129 134))
   :test #'equalp
   :multiple-values t)

  (ptester:test
   (list #(255 254 97 0 98 0 99 0) t)
   (iconv:iconv :utf-8 :utf-16 #(97 98 99))
   :test #'equalp
   :multiple-values t)

  (ptester:test
   (list #(255 254 0 0 97 0 0 0 98 0 0 0 99 0 0 0) t)
   (iconv:iconv :utf-8 :utf-32 #(97 98 99))
   :test #'equalp
   :multiple-values t)
  )

(defun loop-test ()
  (dotimes (_ 100000)
    (iconv:iconv "UTF-8" "EUC-JP" #(227 129 130 227 129 132 227 129 134))))
