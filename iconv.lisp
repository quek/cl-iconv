(defpackage :koto.iconv
  (:nicknames :iconv)
  (:use :cl)
  (:export
   :iconv))

(in-package :iconv)

(defun get-errno ()
  #+sbcl
  (sb-alien:get-errno)
  #+cmu
  (unix:unix-errno)
  #-(or sbcl cmu)
  (cffi:mem-aref  (cffi:foreign-funcall "__errno_location" :pointer) :int 0)
  )

(defconstant E2BIG
  #+sbcl
  SB-POSIX:E2BIG
  #+cmu
  UNIX:E2BIG
  #-(or sbcl cmu)
  7
  "The output buffer has no more room for the next converted character.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:defcfun "iconv_open" :pointer
    (tocode :string)
    (fromcode :string))

  (cffi:defcfun "iconv_close" :int
    (cd :pointer))

  (cffi:defcfun ("iconv" %iconv) :unsigned-int
    (cd :pointer)
    (inbuf :pointer)
    (inbytesleft :pointer)
    (outbuf :pointer)
    (outbytesleft :pointer)))

(defconstant +error-return+
  (let ((cd (iconv-open "UTF-8" "EUC-JP"))
        (len 3))
    (unwind-protect
         (cffi:with-foreign-objects
             ((inbuffer :unsigned-char len)
              (outbuffer :unsigned-char len)
              (in-ptr :pointer)
              (out-ptr :pointer)
              (inbytesleft  :unsigned-int)
              (outbytesleft :unsigned-int))
           (loop for i from 0 below len
              for v in '(227 0 0)
              do (setf (cffi:mem-aref inbuffer :unsigned-char i) v))
           (setf (cffi:mem-aref in-ptr :pointer 0) inbuffer
                 (cffi:mem-aref out-ptr :pointer 0) outbuffer
                 (cffi:mem-aref inbytesleft :unsigned-int 0) len
                 (cffi:mem-aref outbytesleft :unsigned-int 0) len)
           (%iconv cd in-ptr inbytesleft out-ptr outbytesleft))
      (iconv-close cd))))

(defmacro with-iconv-cd ((cd from to) &body body)
  `(let ((,cd (iconv-open (string ,to) (string ,from))))
     (unwind-protect
          (progn ,@body)
       (iconv-close ,cd))))

(defun iconv (from-code to-code from-vector
	      &optional (error-value #.(char-code #\?)))
  (with-iconv-cd (cd from-code to-code)
    (let* ((in-len (length from-vector))
	   (out-len (* in-len 2))
	   (out (make-array out-len
			    :element-type '(unsigned-byte 8)
			    :fill-pointer 0
			    :adjustable t))
           (success-p t))
      (cffi:with-foreign-objects
          ((inbuffer :unsigned-char in-len)
           (outbuffer :unsigned-char out-len)
           (in-ptr :pointer)
           (out-ptr :pointer)
           (inbytesleft  :unsigned-int)
           (outbytesleft :unsigned-int))
        (loop for i from 0 below in-len
           do (setf (cffi:mem-aref inbuffer :unsigned-char i)
                    (aref from-vector i)))
        (setf (cffi:mem-aref in-ptr :pointer 0) inbuffer
              (cffi:mem-aref out-ptr :pointer 0) outbuffer
              (cffi:mem-aref inbytesleft :unsigned-int 0) in-len
              (cffi:mem-aref outbytesleft :unsigned-int 0) out-len)
        (labels ((current ()
                   (- in-len
                      (cffi:mem-aref inbytesleft :unsigned-int 0)))
                 (copy-to-out ()
                   (loop for i from 0
                      below (- out-len
                               (cffi:mem-aref outbytesleft :unsigned-int 0))
                      do (vector-push-extend
                          (cffi:mem-aref outbuffer :unsigned-char i)
                          out)))
                 (run ()
                   (let ((ret (%iconv cd
                                      in-ptr
                                      inbytesleft
                                      out-ptr
                                      outbytesleft)))
                     (when (equalp ret +error-return+)
                       (if (= E2BIG (get-errno))
                           (progn
                             (copy-to-out)
                             (setf (cffi:mem-aref out-ptr :pointer 0) outbuffer
                                   (cffi:mem-aref outbytesleft :unsigned-int 0)
                                   out-len))
                           (progn
                             ;;(warn "~x is invald."
                             ;;      (cffi:mem-aref 
                             ;;       (cffi:mem-aref in-ptr :pointer 0)
                             ;;       :unsigned-char 0))
                             (setf success-p nil)
                             (setf (cffi:mem-aref inbuffer
                                                  :unsigned-char (current))
                                   error-value)))
                       (run)))))
          (run)
          (copy-to-out)))
      (values out success-p))))
