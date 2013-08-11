(defpackage :koto.iconv
  (:nicknames :iconv)
  (:use :cl)
  (:export :iconv
           :iconv-compat
           :iconv-error
           :iconv-open-error
           :iconv-unknown-encoding-error
           :iconv-invalid-multibyte
           :iconv-to-string
           :iconv-from-string))

(in-package :iconv)

(defvar *report-bytes-count* 10
  "The number of bytes that will be reported when displaying an error.")

(cffi:define-foreign-library libiconv
  (:darwin "libiconv.dylib")
  ((and :unix (not :linux)) "libiconv.so"))

(cffi:use-foreign-library libiconv)

(define-condition iconv-error (error)
  ()
  (:documentation "Parent for all iconv errors"))

(define-condition iconv-open-error (iconv-error)
  ()
  (:documentation "Error that is raised when ICONV-OPEN returns as error."))

(define-condition iconv-unknown-encoding-error (iconv-error)
  ()
  (:documentation "Error that is raised if the encoding is unknown."))

(define-condition iconv-invalid-multibyte (iconv-error)
  ((buffer :initarg :buffer
           :reader iconv-invalid-multibyte-buffer
           :documentation "The buffer that is being converted. This is a CFFI native buffer.")
   (index :type integer
          :initarg :index
          :reader iconv-invalid-multibyte-index
          :documentation "Index into the source buffer where the error was encountered")
   (at-end-p :initform nil
             :initarg :at-end-p
             :reader iconv-invalid-multibyte-at-end-p
             :documentation "True if the encoding error happened at the end of the buffer"))
  (:report (lambda (condition out)
             (let* ((index (iconv-invalid-multibyte-index condition))
                    (buf (cffi:convert-from-foreign (iconv-invalid-multibyte-buffer condition)
                                                    (list :array :unsigned-char index)))
                    (l (max (- index *report-bytes-count*) 0)))
               (format out "Error decoding buffer at index ~a. Error occurred ~:[mid-buffer.~;at the end of the buffer.~]
Previous ~a bytes leading up to this error: ~s"
                       index (iconv-invalid-multibyte-at-end-p condition) (- index l) (subseq buf l)))))
  (:documentation "Error that is raised when conversion encounters an illegal multibyte sequence"))

(defun get-errno ()
  (iolib.syscalls:errno))

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
          (error 'iconv-open-error)))
    result))

(defconstant +error-return+ (1- (ash 1 (* (cffi:foreign-type-size :unsigned-long) 8))))

(defmacro with-iconv-cd ((cd from to) &body body)
  `(let ((,cd (iconv-open (string ,to) (string ,from))))
     (unwind-protect
          (progn ,@body)
       (iconv-close ,cd))))

(defun read-new-replacement-and-offset ()
  (format t "Enter replacement (a single character, codepoint, a string, or nil for no replacement): ")
  (finish-output)
  (let ((replacement (read)))
    (format t "Enter number of bytes to skip: ")
    (finish-output)
    (let ((number-of-bytes (read)))
      (list replacement number-of-bytes))))

(defun iconv (from-code to-code from-vector)
  (with-iconv-cd (cd from-code to-code)
    (let* ((in-len (length from-vector))
	   (out-len (* in-len 2))
	   (out (make-array out-len
			    :element-type '(unsigned-byte 8)
			    :fill-pointer 0
			    :adjustable t))
           end)
      (cffi:with-foreign-objects
          ((inbuffer :unsigned-char in-len)
           (outbuffer :unsigned-char out-len)
           (in-ptr :pointer)
           (out-ptr :pointer)
           (inbytesleft  :unsigned-long)
           (outbytesleft :unsigned-long))
        (loop for i from 0 below in-len
           do (setf (cffi:mem-aref inbuffer :unsigned-char i)
                    (aref from-vector i)))
        (setf (cffi:mem-aref in-ptr :pointer 0) inbuffer
              (cffi:mem-aref out-ptr :pointer 0) outbuffer
              (cffi:mem-aref inbytesleft :unsigned-long 0) in-len
              (cffi:mem-aref outbytesleft :unsigned-long 0) out-len)
        (labels ((current ()
                   (- in-len
                      (cffi:mem-aref inbytesleft :unsigned-long 0)))

                 (has-more-p ()
                   (and (not end)
                        (plusp (cffi:mem-aref inbytesleft :unsigned-long 0))))

                 (copy-to-out ()
                   (loop for i from 0
                      below (- out-len
                               (cffi:mem-aref outbytesleft :unsigned-long 0))
                      do (vector-push-extend
                          (cffi:mem-aref outbuffer :unsigned-char i)
                          out)))

                 (copy-to-out-and-clear-out-buffer ()
                   (copy-to-out)
                   (setf (cffi:mem-aref out-ptr :pointer 0) outbuffer
                                          (cffi:mem-aref outbytesleft :unsigned-long 0)
                                          out-len))

                 (increment-in-ptr (delta)
                   (let* ((remaining (cffi:mem-ref inbytesleft :unsigned-long)))
                     (setq delta (min delta remaining))
                     (setf (cffi:mem-ref in-ptr :pointer)
                           (cffi-sys:make-pointer (+ (cffi-sys:pointer-address (cffi:mem-ref in-ptr :pointer)) delta)))
                     (setf (cffi:mem-ref inbytesleft :unsigned-long) (- remaining delta))))

                 (read-next-part ()
                   (restart-case
                       (let ((ret (%iconv cd
                                          in-ptr
                                          inbytesleft
                                          out-ptr
                                          outbytesleft)))
                         (when (= ret +error-return+)
                           (let ((errno (get-errno)))
                             (cond ((= errno iolib.syscalls:e2big)
                                    (copy-to-out-and-clear-out-buffer))
                                   ((= errno iolib.syscalls:eilseq)
                                    (copy-to-out-and-clear-out-buffer)
                                    (error 'iconv-invalid-multibyte
                                           :buffer inbuffer
                                           :index (current)
                                           :at-end-p nil))
                                   ((= errno iolib.syscalls:einval)
                                    (copy-to-out-and-clear-out-buffer)
                                    (setq end t)
                                    (error 'iconv-invalid-multibyte
                                           :buffer inbuffer
                                           :index (current)
                                           :at-end-p t))))))
                     (iconv-cont ()
                       :report "Continue decoding, skipping the current invalid byte"
                       (when (has-more-p)
                         (increment-in-ptr 1))
                       nil)
                     (iconv-cont-with-args (&optional replacement (delta 1))
                       :report "Continue decoding, specifying a replacement sequence and a skip length"
                       :interactive read-new-replacement-and-offset
                       (when (has-more-p)
                         (increment-in-ptr delta))
                       (when replacement
                         (map nil #'(lambda (v)
                                      (vector-push-extend v out))
                              (iconv-from-string to-code
                                                 (etypecase replacement
                                                   (character (princ-to-string replacement))
                                                   (integer (princ-to-string (code-char replacement)))
                                                   (string replacement)
                                                   (sequence (map 'string #'identity replacement)))))
                         nil))
                     (iconv-finish ()
                       :report "Finish decoding"
                       (setq end t))))

                 (run ()
                   (loop
                      while (has-more-p)
                      do (read-next-part))))

          (run)
          (copy-to-out)))
      out)))

(defun iconv-compat (from-code to-code from-vector
                     &optional (error-value #.(char-code #\?)))
  "The old behaviour of ICONV implemented in terms of the new conditions-based version."
  (let ((illegal-code-seen-p nil))
    (handler-bind ((iconv-invalid-multibyte
                    #'(lambda (condition)
                        (declare (ignore condition))
                        (setq illegal-code-seen-p t)
                        (invoke-restart 'iconv-cont-with-args error-value))))
      (let ((result (iconv from-code to-code from-vector)))
        (values result illegal-code-seen-p)))))

(defun iconv-to-string (from-code from-vector)
  "Decode a byte array into a string."
  (let ((result (iconv:iconv from-code :utf-32be from-vector)))
    (unless (zerop (mod (length result) 4))
      (error "UTF-32BE output length is not divisible by 4: ~a" (length result)))
    (with-output-to-string (out)
      (dotimes (i (/ (length result) 4))
        (write-char (code-char (logior (ash (aref result (* i 4)) 24)
                                       (ash (aref result (+ (* i 4) 1)) 16)
                                       (ash (aref result (+ (* i 4) 2)) 8)
                                       (aref result (+ (* i 4) 3))))
                    out)))))

(defun iconv-from-string (to-code string)
  "Encode a string into a byte array."
  (let ((buf (make-array (* (length string) 4) :element-type '(unsigned-byte 8))))
    (loop
       for i from 0 below (length string)
       for ch across string
       do (let ((code (char-code ch)))
            (setf (aref buf (* i 4)) (logand (ash code -24) #xFF))
            (setf (aref buf (+ (* i 4) 1)) (logand (ash code -16) #xFF))
            (setf (aref buf (+ (* i 4) 2)) (logand (ash code -8) #xFF))
            (setf (aref buf (+ (* i 4) 3)) (logand code #xFF))))
    (iconv :utf-32be to-code buf)))

(defun test-print-array (v)
  (format t "~{~2,'0x ~}" (coerce v 'list)))

(defun test-iconv (&optional (function #'iconv))
  (test-print-array (funcall function :utf-8 :utf-16be #(#xe2 #x98 #x83 ; snowman
                                                         #x01
                                                         #xe2 #x98 ; broken snowman
                                                         #x02
                                                         #xe2 #x98 #x83 ; snowman
                                                         #xe2 #x97))))
