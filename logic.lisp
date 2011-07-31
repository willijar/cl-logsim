;; Logic Gate entities
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

(defclass logic(entity with-inputs with-outputs with-delay)
  ()
  (:documentation "base class for all logic (entities with inputs)"))

(defclass logic-function-gate(logic)
  ((inverted-inputs :type (vector bit *) :reader inverted-inputs))
  (:documentation "Class for all basic logic functions"))

(defmethod initialize-instance :before ((g logic-function-gate)
                                       &key (inputs 2) &allow-other-keys)
  (setf (slot-value g 'inverted-inputs)
        (etypecase inputs
          (string (map 'bit-vector
                       #'(lambda(s) (ecase s (#\i 1) (#\n 0)))
                       inputs))
          (bit-vector inputs)
          (integer
           (make-array inputs :element-type 'bit :initial-element 0)))))

(defmethod input-initialization((gate logic-function-gate))
  (let ((c -1))
    (map 'list
         #'(lambda(e) (declare (ignore e)) (incf c))
         (inverted-inputs gate))))

(defmethod calculate-output-signals ((gate logic-function-gate))
  (make-array 1
              :element-type 'bit
              :initial-element
              (reduce (slot-value gate 'logic-function)
                      (bit-xor (signal-value (inputs gate))
                               (inverted-inputs gate)))))1

(defclass and-gate(logic-function-gate)
  ((logic-function :initform #'logand :allocation :class)))

(defclass or-gate(logic-function-gate)
  ((logic-function :initform #'logior :allocation :class)))

(defclass xor-gate(logic-function-gate)
  ((logic-function :initform #'logxor :allocation :class)))

(defclass nand-gate(logic-function-gate)
  ((logic-function :initform #'lognand :allocation :class)))

(defclass nor-gate(logic-function-gate)
  ((logic-function :initform #'lognor :allocation :class)))

(defclass not-gate(logic-function-gate)
   ())

(defmethod calculate-output-signals((gate not-gate))
  (bit-not (signal-value (inputs gate))))

(defclass truth-table-gate(logic)
  ((input-initialization :initarg :inputs :reader input-initialization)
   (output-initialization :initarg :outputs :reader output-initialization)
   (truth-table :type vector :reader truth-table))
  (:documentation "A gate represented by a truth table"))

(defun bit-vector-to-integer(bv)
  (let ((s 0))
    (loop :for b :across bv
       :do (setf s (ash s 1))
       :unless (zerop b) :do (incf s))
    s))

(defmethod calculate-outputs((gate truth-table-gate))
  (aref (truth-table gate) (bit-vector-to-integer (signal-value (inputs gate)))))
