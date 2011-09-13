;; Signal source entities
;; Copyright (C) 2011 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;;; Copying:

;; This file is part of LOGSIM

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; FOOBAR is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


(in-package :logsim)

(defvar *default-clk-period* 1)

(defclass source(entity with-outputs)
  ((signal-sequence :type vector :reader signal-sequence
                    :documentation "Sequence of values for this
signal.  These are specified as a duration followed by the signal
values. For non-periodic signals the last time period is ignored.")
   (signal-index :type fixnum :initform 0 :accessor signal-index
                   :documentation "Current position in sequence")
   (periodic :initarg :periodic :type boolean :initform nil :reader periodic)))

(defun duration(source)
  (unless (periodic source)
    (reduce #'+ (signal-sequence source) :key #'car)))

(defmethod initialize-instance :after ((s source)
                                       &key sequence &allow-other-keys)
  (let ((n (length (outputs s))))
    (setf (slot-value s 'signal-sequence)
          (map 'vector
               #'(lambda(signal)
                   (multiple-value-bind(delay signal)
                       (if (consp signal)
                           (values (car signal) (cdr signal))
                           (values 1 signal))
                     (cond
                       ((and (= n 1) (not (typep signal 'sequence)))
                        (cons delay
                              (make-array 1 :element-type 'bit
                                          :initial-element signal)))
                       ((= (length signal) (length (outputs s)))
                        (cons delay (coerce signal `(bit-vector ,n))))
                       (t
                        (error "Signal vector length does not match signal output length")))))
               sequence)))
  (reset s))

(defmethod inputs-changed((s source) &optional dummy)
  (declare (ignore dummy))
  (with-slots((idx signal-index) (seq signal-sequence)) s
    (incf idx)
    (when (>= idx (length seq))
      (if (periodic s)
          (setf idx 0)
          (return-from inputs-changed)))
    ;; inform connected entities and reschedule if necessary
    (change-outputs (cdr (aref seq idx)) s)
    (schedule (car (aref seq idx)) #'(lambda() (inputs-changed s)))))

(defmethod reset((s source))
  (setf (signal-index s) 0)
  (let ((v (aref (signal-sequence s) 0)))
    (setf (signal-value (outputs s)) (cdr v))
    (schedule (car v) #'(lambda() (inputs-changed s)))))

(defun make-clock(&key (period *default-clk-period*) (name 'CLK))
  "Clock is just a periodic signal with one output"
  (make-instance 'source
                 :name name
                 :outputs '(CLK)
                 :sequence `((,(/ period 2) . #*0) (,(/ period 2) . #*1))
                 :periodic t))

(defmethod reset((entities hash-table))
  (maphash #'(lambda(k e)
               (declare (ignore k))
               (when (typep e 'source) (reset e)))
           entities)
  (maphash #'(lambda(k e)
                (declare (ignore k))
                (unless (typep e 'source) (reset e)))
           entities))