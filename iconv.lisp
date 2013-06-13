(defpackage :koto.iconv
  (:nicknames :iconv)
  (:use :cl)
  (:export
   :iconv))

(in-package :iconv)

(cffi:define-foreign-library libiconv
  (:darwin "libiconv.dylib")
  (:unix "libiconv.so"))

(cffi:use-foreign-library libiconv)

(define-condition iconv-error (error)
  ()
  (:documentation "Error that is raised when ICONV-OPEN returns as error."))

(define-condition iconv-unknown-encoding-error (error)
  ()
  (:documentation "Error that is raised if the encoding is unknown."))

(defun get-errno ()
  (iolib.syscalls:errno))

(defconstant E2BIG iolib.syscalls:e2big
  "The output buffer has no more room for the next converted character.")

(cffi:defcfun ("iconv_open" %iconv-open) :pointer
  (tocode :string)
  (fromcode :string))

(cffi:defcfun "iconv_close" :int
  (cd :pointer))

(cffi:defcfun ("iconv" %iconv) :unsigned-long
  (cd :pointer)
  (inbuf :pointer)
  (inbytesleft :pointer)
  (outbuf :pointer)
  (outbytesleft :pointer))

(defun iconv-open (tocode fromcode)
  (let ((result (%iconv-open tocode fromcode)))
    (when (= (cffi:pointer-address result)
             (1- (ash 1 (* (cffi:foreign-type-size :pointer) 8))))
      (if (= (get-errno) iolib.syscalls:einval)
          (error 'iconv-unknown-encoding-error)
          (error 'iconv-error)))
    result))

(defconstant +error-return+ (1- (ash 1 (* (cffi:foreign-type-size :unsigned-long) 8))))

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
