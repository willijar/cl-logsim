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

(defmethod initialize-inputs((entity logic) &key (inputs 2) &allow-other-keys)
  (integer-sequence (if (integerp inputs) inputs (length inputs))))

(defclass logic-function-gate(logic)
  ((inverted-inputs :type (vector bit *) :reader inverted-inputs))
  (:documentation "Class for all basic logic functions"))

(defmethod initialize-instance :after ((g logic-function-gate)
                                       &key (inputs 2) &allow-other-keys)
  (setf (slot-value g 'inverted-inputs)
        (etypecase inputs
          (string (map 'bit-vector
                       #'(lambda(s) (ecase s (#\i 1) (#\n 0)))
                       inputs))
          (bit-vector inputs)
          (integer
           (make-array inputs :element-type 'bit :initial-element 0)))))

(defmethod calculate-output-signals ((gate logic-function-gate))
  (make-array 1
              :element-type 'bit
              :initial-element
              (reduce (slot-value gate 'logic-function)
                      (bit-xor (signal-value (inputs gate))
                               (inverted-inputs gate)))))

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
  ((truth-table :type vector :reader truth-table))
  (:documentation "A gate represented by a truth table"))

(defmethod initialize-inputs((gate truth-table-gate)
                             &key inputs &allow-other-keys)
  inputs)

(defmethod initialize-outputs((gate truth-table-gate)
                              &key outputs &allow-other-keys)
  outputs)

(defmethod calculate-output-signals((gate truth-table-gate))
  (aref (truth-table gate) (bit-vector-to-integer (signal-value (inputs gate)))))

(defclass decoder(logic)
  ()
  (:documentation "Line Decoder class - takes inputs as number of address lines"))

(defmethod initialize-outputs((decoder decoder) &key (inputs 2) &allow-other-keys)
  (integer-sequence (ash 1 inputs)))

(defmethod calculate-output-signals((decoder decoder))
  (let ((op (make-array (length (outputs decoder)) :element-type 'bit
                        :initial-element 0)))
    (setf (aref op (bit-vector-to-integer (signal-value (inputs decoder)))) 1)
    op))

(defclass multiplexer(logic)
  ((n :type fixnum
      :documentation "Number of address lines in this multiplexer"))
  (:documentation "Multiplexer - takes inputs as number of signal lines"))

(defmethod initialize-inputs((m multiplexer) &key inputs &allow-other-keys)
  (let ((n (setf (slot-value m 'n) (ceiling (log inputs 2)))))
    (flet ((iname(pfx num) (intern (format nil "~A~D" pfx num))))
      (nconc
       (mapcar #'(lambda(n) (iname #\S n)) (integer-sequence n))
       (mapcar #'(lambda(n) (iname #\I n)) (integer-sequence (ash 1 n)))))))

(defmethod calculate-output-signals((m multiplexer))
  (let* ((n (slot-value m 'n))
         (iv (signal-value (inputs m)))
         (addr (bit-vector-to-integer (subseq iv 0 n))))
      (make-array 1 :element-type 'bit
                  :initial-element (aref iv (+ n addr)))))

(defun logic-equal-p(A B)
  "Return true if two logic expressions are equal"
  (if (and (listp A) (listp B))
      (and (eql (car A) (car B))
           (= (length A) (length B))
           (every
            #'(lambda(Aelement) (find Aelement (cdr B) :test #'logic-equal-p))
            (cdr A)))
      (eql A B)))

(defun build-logic(expr &optional (env (make-hash-table)))
  "Construct logic using gates - return the output and a list of gates
as values. Symbols will connect to connections with given name"
  (cond
    ((typep expr '(or output bit)) expr) ; primitives
    ((typep expr 'symbol)  (gethash expr env)) ; named outputs
    ((not (listp expr)) (error "Unknown logic term ~A" expr))
    ((and (symbolp (car expr))
          (char= (char (symbol-name (car expr)) 0) #\=)) ; nameing a primitivr
     (unless (= (length expr) 2)
       (error "Invalid Expression ~A 1 argument expected" expr))
     (multiple-value-bind(output gates) (build-logic (second expr) env)
       (setf (gethash (intern (subseq (symbol-name (car expr)) 1)) env)
             output)
       (values output gates)))
    ((eql (car expr) 'not) ; not gate
     (unless (= (length expr) 2)
          (error "Invalid Expression ~A 1 argument expected" expr))
        (let ((gate (make-instance 'not-gate)))
          (multiple-value-bind(output gates)
              (build-logic (second expr) env)
            (connect output (aref (inputs gate) 0))
            (values (aref (outputs gate) 0) (cons gate gates)))))
    ((member (car expr) '(or and nor nand xor nxor)) ; other gates
     (unless (> (length expr) 2)
          (error "Invalid Expression ~A >1 argument expected" expr))
     (let ((inputs-arg nil)
           (args nil))
       ;; we convert not into inverting inputs
       (dolist(expr (cdr expr))
         (if  (and (listp expr) (eql (car expr) 'not) (= 2 (length expr)))
              (progn
                (pushnew #\i inputs-arg)
                (push (second expr) args))
              (progn
                (pushnew #\n inputs-arg)
                (push expr args))))
       (let* ((new-gate
               (make-instance (intern (format nil "~A-GATE" (car expr)))
                              :inputs (coerce 'string (reverse inputs-arg))))
                 (gates (list new-gate)))
         (setf args (reverse args))
         (dotimes(i (length args))
           (multiple-value-bind(output new-gates)
               (build-logic (elt args i) env)
             (setf gates (nconc gates new-gates))
             (connect output (aref (inputs new-gate) i))))
         (values (aref (outputs new-gate) 0) gates))))
    ( ;; all other logiv units
     (let ((this-gate (if (symbolp (car expr))
                          (make-instance (car expr))
                          (apply #'make-instance (car expr))))
           (gates))
       (loop
          :for r :on (cdr expr) :by #'cddr
          :do (let ((ipname (car r))
                    (expr (cadr r)))
                (multiple-value-bind(output new-gates) (build-logic expr env)
                  (setf gates (nconc gates new-gates))
                  (connect output (find ipname (inputs this-gate) :key 'name)))))
       (values (aref (outputs this-gate) 0) (cons this-gate gates))))))

(defun build-logic-block(expr &key name)
"Build a logic block with one output from expression. Undefined variables will be mapped onto inputs on the block. "
)

;; (defun logic-match(pattern input)
;;   "This will attempt to match input logic expression against a logic
;; pattern It will return an alist of the free symbols in patterns with
;; the mathching components of the input. Argument order does not matter
;; in the matches."
;;   (let ((vars nil))
;;     (labels
;;         ((save-match(symbol expr)
;;            (when (assoc symbol vars)
;;              (error "Duplicate free variable ~A in logic pattern" symbol))
;;            (push (cons symbol expr) vars))
;;          (do-match(pattern input)
;;            (etypecase pattern
;;              (symbol (save-match pattern input))
;;              (list
;;               (unless (and (listp input)
;;                            (or (and (listp (car pattern))
;;                                     (member (car input) (car pattern)))
;;                                (eql (car pattern) (car input))))
;;                 (return-from logic-match nil))
;;               ;; we know we have matching function - check args
;;               (let ((unmatched (rest pattern)))
;;               (dolist(iparg (rest input))
;;                 (dolist(patarg (rest unmatched))
;;                   (when (do-match patarg iparg)
;;                     (setf unmatched (delete patarg unmatched))
;;                     (return))))
;;               (when unmatched (return-from logic-match nil)))))))
;;       (do-match pattern input)
;;       vars))