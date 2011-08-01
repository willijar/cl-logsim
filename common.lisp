;; Some commonly used definitions and protocols
;; Copyright (C) 2011 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:


;;; Code:

(in-package :logsim)

;; Basic types

(defconstant +c+ 299792458d0 "Speed of Light in m/sec")

;; base class for in simulation errors (not program errors)
(define-condition simulation-condition(condition)())

;; Some basic macros
(defmacro while (test &body body)
  "A while loop - repeat body while test is true"
  `(do ()
    ((not ,test))
    ,@body))

(defmacro until (test &body body)
  "Repeat body until test returns true"
  `(do ()
    (,test)
    ,@body))

(defmacro when-bind ((var expr) &body body)
  "Bind VAR to VALUE of expression, execute body if true"
  `(let ((,var ,expr))
    (when ,var
      ,@body)))

(defmacro filter(test lst &key (key '#'identity))
  "Return a list of the elements in `lst` for which `test` (applied to `key`)
is true.

Arguments:

- `test`: a designator for a function of one argument which returns a
          generalised boolean
- `lst`: a proper list
- `key`: a designator for a function of one argument

Returns:

- `result`: a list"
  `(mapcan #'(lambda(it)
               (when (funcall ,test (funcall ,key it))
                 (list it)))
    ,lst))

(defmacro defenumeration (typename (&rest items))
  (let ((items (loop :for item :in items
                     :for count :from 0
                     :collect (if (consp item) item (list item count)))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
      ,@(loop :for item :in items
              :collect
              `(defconstant ,(first item) ,@(rest item)))
      ,@(if (listp typename)
            `((deftype ,(first typename)() ',@(rest typename))
              (defvar ,(first typename) ',(mapcar #'first items)))
            `((deftype ,typename () '(member ,@(mapcar #'second items)))
              (defvar ,typename ',(mapcar #'first items)))))))

(defgeneric name(entity)
  (:documentation "Return a descriptive name of an entity - used in tracing")
  (:method((obj standard-object))
    (class-name (class-of obj))))

(defgeneric start(entity &key &allow-other-keys)
  (:documentation "Start an entity"))

(defgeneric stop(entity &key abort)
  (:documentation "Stop a running entity.
 If keyword abort is true - also abort current action"))

(defgeneric reset(entity)
  (:documentation "REset an entity to initial conditions")
  (:method((seq sequence))
    (map 'nil #'reset seq)))

(defmacro trace-accessor((slotname (objvar type)
                          &optional (slotvar (gensym))) &rest body)
  "Define method to add tracing code to standard slot writer function.
It is assumed that slot and accessor have the same name."
  `(defmethod (setf ,slotname)(,slotvar (,objvar ,type))
     ,@(or body
           `((format *standard-trace* "~A ~A ~A->~A~%"
                     ,objvar ',slotname (slot-value  ,objvar ',slotname)
                     ,slotvar)))
     (setf (slot-value ,objvar ',slotname) ,slotvar)))

(defmacro untrace-accessor((slotname (objvar type)))
   "Define method to remove tracig code and replace with a setf slot
It is assumed that slot and accessor have the same name."
  (let ((gv (gensym)))
  `(defmethod (setf ,slotname)(,gv (,objvar ,type))
     (setf (slot-value ,objvar ',slotname) ,gv))))

(defun cl-user::print-eng(os arg &optional colon-p at-p
                 (d 2) (padchar #\space) (exponentchar #\e))
  "Formatter which outputs its numerical argument `arg` in engineering format
to stream `os`.
It takes arguments d,padchar,exponentchar where
d is the number of decimal places to display after the decimal point
padchar is the character to pad the start of the number
exponentchar is the character to use to display between radix and exponent
It also takes the : modifier which will cause it to output the exponent
as an SI units prefix rather than a number.

Arguments:

- `os`: an output stream designator
- `arg`: a number
- `colon-p`: a generalised boolean (default false)
- `at-p`: a generalised boolean (default false) - ignored
- `d`: an integer (default 2)
- `padchar`: a character (default `space`)
- `exponentchar`: a character (default `e`))

Result:

nil

Examples:

`(format nil \"~/print-eng/\" 35000) => \"35.00e+3\"`
"
  (declare (ignore at-p))
  (if (numberp arg)
      (let* ((units "YZEPTGMk munfazy")
             ;; note use u instead of \mu for 1e-6 so utf-8 not needed
             (order (if (zerop arg) 0 (floor (log arg 10) 3)))
             (scale (* 3 order))
             (radix-format
              (if (or (zerop d) (integerp arg))
                  (format nil "~~,'~CD" padchar)
                  (format nil "~~,~@[~D~],,,'~CF"
                           d  padchar)))
             (radix (/ arg (expt 10 scale))))
        (when (zerop d) (setf radix (round radix)))
        (if (and colon-p (< -1 (- 8 order) (length units)))
            (format os "~@? ~:[~C~;~]"
                    radix-format
                    radix
                    (zerop scale)
                    (char units (- 8 order)))
            (format os "~@?~:[~C~@D~;~]"
                    radix-format
                    radix
                    (zerop scale)
                    exponentchar
                    scale)))
      (princ arg os)))

(defun integer-sequence(end &key (start 0))
  (loop :for x :from start :below end
     :collect x))

(defun bit-vector-to-integer(bv)
  (let ((s 0))
    (loop :for b :across bv
       :do (setf s (ash s 1))
       :unless (zerop b) :do (incf s))
    s))

(defun integer-to-bit-vector(int no-bits)
  (let ((result (make-array no-bits :element-type 'bit)))
    (dotimes(i no-bits)
      (setf (aref result (- no-bits i 1)) (ldb (byte 1 i) int)))
    result))
