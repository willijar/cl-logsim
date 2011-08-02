;; Finite State Machine entities
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

(defclass fsm(entity with-inputs with-outputs with-edge-detection)
  ((control :initform 1 :type bit :initarg :control
            :initarg :edge
            :documentation "Clock edge transition control - +ve if 1")
   (state :type bit-vector :accessor state
          :documentation "Current state vector")
   (initial-state-vector :initarg :initial-state-vector
                         :type bit-vector :reader initial-state-vector
                         :documentation "Initial state vector upon reset")
   (transitions :type vector
                :documentation "transitions table looked up by state" )
   (clk :type bit :documentation "Last read clock value"))
  (:documentation "An algorithmic state machine entity"))

(defmethod initialize-inputs  ((fsm fsm) &key inputs &allow-other-keys)
  ;;Add clock as 0th input
  `(CLK ,@inputs))

(defmethod initialize-outputs ((fsm fsm) &key no-state-bits outputs &allow-other-keys)
  ;; add state bits as outputs - useful for tracing purposes
  (nconc
   (mapcar #'(lambda(n) (intern (format nil "X~D" n)))
           (integer-sequence no-state-bits))
   outputs))

(defmethod initialize-instance :after ((fsm fsm) &key no-state-bits state-table &allow-other-keys)
  (with-slots(initial-state-vector transitions) fsm
   (if (slot-boundp fsm 'initial-state-vector)
      (unless (= no-state-bits (length (initial-state-vector fsm)))
        (error "Malformed Initial State vector: length is not ~D" no-state-bits))
      (setf initial-state-vector (caar state-table)))
   (setf (state fsm) initial-state-vector)
   (setf transitions (make-array (ash 1 no-state-bits) :initial-element nil))
  (dolist(row state-table)
    (let ((i (bit-vector-to-integer (car row))))
      (when (aref transitions i)
        (error "Duplicate row in tsable for state ~A" (car row)))
      (setf (aref transitions i) (cdr row))))))

(defmethod reset((fsm fsm))
  (setf (state fsm) (initial-state-vector fsm)))

(defun clock-edge-p(fsm)
  (let ((new-clk (signal-value (aref (inputs fsm) 0))))
    (with-slots(clk control) fsm
      (unless (slot-boundp fsm 'clk) (setf clk new-clk))
      (when (and (/= new-clk clk) (= new-clk control))
        (setf clk new-clk)
        t))))

(defgeneric next-state(fsm input-vector transitions)
  (:documentation "Return the next state for a fsm given the input vector"))

(defgeneric output-vector(fsm input-vector transitions)
  (:documentation "Return the output signal vector for an fsm given
  the input vector"))

(defmethod calculate-output-signals((fsm fsm))
  (with-slots(state transitions) fsm
    (let ((row (aref transitions (bit-vector-to-integer state)))
          (input-vector  (subseq (signal-value (inputs fsm)) 1)))
      (when (clock-edge-p fsm)
        (setf state (next-state fsm input-vector)
              row (aref transitions (bit-vector-to-integer state))))
      (concatenate 'bit-vector state (output-vector fsm input-vector row)))))

(defclass mealy-model(fsm)
  ()
  (:documentation "An FSM based on the Mealy model"))

(defmethod next-state((fsm mealy-model) input-vector transitions)
  (second (find input-vector transitions :key #'first :test #'equal)))

(defmethod output-vector((fsm mealy-model) input-vector transitions)
  (third (find input-vector transitions :key #'first :test #'equal)))

(defclass moore-model(fsm)
  ()
  (:documentation "An FSM based on the Moore model"))

(defmethod next-state((fsm moore-model) input-vector transitions)
  (cdr (find input-vector (cdr transitions) :key #'car :test #'equal)))

(defmethod output-vector((fsm moore-model) input-vector transitions)
  (car transitions))

(defclass asm(fsm)
  ()
  (:documentation "An Algorithmic State Machine Model"))

