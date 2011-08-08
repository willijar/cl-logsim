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
   (state-data :type list :reader state-data :initarg :state-data
                :documentation "The state transition table" )
   (clk :type bit :documentation "Last read clock value"))
  (:documentation "An finite state machine entity"))

(defmethod initialize-inputs  ((fsm fsm) &key inputs &allow-other-keys)
  ;;Add clock as 0th input
  `(CLK ,@inputs))

(defmethod initialize-outputs ((fsm fsm) &key no-state-bits outputs &allow-other-keys)
  ;; add state bits as outputs - useful for tracing purposes
  (nconc
   (mapcar #'(lambda(n) (intern (format nil "X~D" n)))
           (integer-sequence no-state-bits))
   outputs))

(defmethod initialize-instance :after ((fsm fsm) &key &allow-other-keys)
  (unless (slot-boundp fsm 'initial-state-vector)
    (setf (slot-value fsm 'initial-state-vector) (caar (state-data fsm))))
  (setf (state fsm) (initial-state-vector fsm)))

(defmethod reset((fsm fsm))
  (setf (state fsm) (initial-state-vector fsm)))

(defun clock-edge-p(fsm)
  (let ((new-clk (signal-value (aref (inputs fsm) 0))))
    (with-slots(clk control) fsm
      (unless (slot-boundp fsm 'clk) (setf clk new-clk))
      (when (and (/= new-clk clk) (= new-clk control))
        (setf clk new-clk)
        t))))

(defgeneric next-state(finite-state-machine input-vector state-data-row)
  (:documentation "Return the next state bit vector given the input-vector and state-data-row for the current state"))

(defgeneric output-vector(finite-state-machine input-vector state-data-row)
  (:documentation "Return the ouput bit vector given the input-vector
  and state-data-row for the current state"))

(defgeneric state-table(entity)
  (:documentation "Return the state table for a FSM in a standardised format.
Each row has initial state, input vector, output state, and output vector
First row has lists of names
other rows have vectors."))

(defgeneric write-state-diagram(entity format &optional stream)
  (:documentation "Write as state diagram out in specified format"))

(defgeneric write-state-table(entity format &optional stream)
  (:documentation "Write state table in given format"))

(defmethod calculate-output-signals((fsm fsm))
  (let ((state (state fsm))
        (input-vector (subseq (signal-value (inputs fsm)) 1)))
    (flet ((row(state)
             (rest (find state (state-data fsm) :test #'equal :key #'car))))
      (let ((row (row state)))
      ;; we check for state change before setting output
      ;; since calculate-output-signals is atomic
      (when (clock-edge-p fsm)
        (unless (= (setf (state fsm) (next-state fsm input-vector row))
                   state)
          (setf row (row (state fsm)))))
      (concatenate 'bit-vector state
                   (output-vector fsm input-vector row))))))

(defclass mealy-model(fsm)
  ()
  (:documentation "An FSM based on the Mealy model"))

(defmethod next-state((fsm mealy-model) input-vector row)
  (dolist(entry row)
    (when (find input-vector (rest entry) :test #'equal :key #'car)
      (return-from next-state (car entry)))))

(defmethod output-vector((fsm mealy-model) input-vector row)
  (dolist(entry row)
    (let ((transition
           (find input-vector (rest entry) :test #'equal :key #'car)))
      (when transition (return-from output-vector (cdr transition))))))

(defmethod state-table :around ((fsm fsm))
  (let ((sn (length (state fsm))))
  (cons
   (list (mapcar #'name (subseq (outputs fsm) sn))
         (map 'list #'name (subseq (inputs fsm) 1))
         (mapcar #'name (subseq (outputs fsm) sn))
         (map 'list #'name (subseq (outputs fsm) 0 sn)))
   (call-next-method))))

(defmethod state-table((fsm mealy-model))
  (mapcan
    #'(lambda(state-row)
        (let ((initial-state (coerce (car state-row) 'list)))
          (mapcan
           #'(lambda(entry)
               (let ((next-state (coerce (car entry) 'list)))
                 (mapcar
                  #'(lambda(transition)
                      (list
                       initial-state
                       (coerce (car transition) 'list)
                       next-state
                       (coerce (cdr transition) 'list)))
                  (cdr entry))))
           (rest state-row))))
    (state-data fsm)))

(defclass moore-model(fsm)
  ()
  (:documentation "An FSM based on the Moore model"))

(defmethod next-state((fsm moore-model) input-vector state-data-row)
  (car (some #'(lambda(entry) (find input-vector (rest entry) :test #'equal))
             state-data-row)))

(defmethod output-vector((fsm moore-model) input-vector state-data-row)
  (declare (ignore input-vector))
  (first state-data-row))

(defmethod state-table((fsm moore-model))
  (mapcan
    #'(lambda(state-row)
        (let ((initial-state (coerce (car state-row) 'list))
              (output (coerce (second state-row) 'list)))
          (mapcan
           #'(lambda(entry)
               (let ((next-state (coerce (car entry) 'list)))
                 (mapcar
                  #'(lambda(transition)
                      (list
                       initial-state
                       (coerce transition 'list)
                       next-state
                       output))
                  (cdr entry))))
           (cddr state-row))))
    (state-data fsm)))

(defmethod write-state-diagram((entity mealy-model) (format (eql :dot))
                               &optional (stream *standard-output*))
  (write-line "digraph state_diagram {
rankdir=LR;
node [shape=circle];" stream)
  (dolist(row (state-data entity))
    (dolist(entry (cdr row))
      (format stream "~/bv/ -> ~/bv/ [label=\"~{~/bv//~/bv/\~^\\n~}\"];~%"
              (car row) (car entry)
              (mapcan #'(lambda(a) (list (car a) (cdr a)))
                      (rest entry)))))
  (write-line "}" stream))

(defmethod write-state-diagram((entity moore-model) (format (eql :dot))  &optional (stream *standard-output*))
  (write-line "digraph state_diagram {
rankdir=LR;
node [shape=circle];" stream)
  (dolist(row (state-data entity))
    (format stream "~/bv/ [label=\"~:*~/bv//~/bv/\"];~%" (first row) (second row)))
  (dolist(row (state-data entity))
    (dolist(entry (cddr row))
      (format stream "~/bv/ -> ~/bv/ [label=\"~{~/bv/~^/~}\"];~%"
              (car row) (car entry) (rest entry))))
  (write-line "}" stream))


;; ASM uses a metalanguage using input and output names
;; names ending in (L) are assumed to assert low otherwise signals assert high
;; The second argument of a state row is a list of asserted outputs for this state
;; the rest are tests on inputs
;; ? tests if its first argument (an input) is asserted - if so it evaluates its second argument otherwise it checks its third argument
;; assert asserts the signals in its argument
;; goto jumps to a new state
;;
;; e.g. (#*000 ()
;; (? X (Z #*000)
;;      ((N O P) (? Y (Q #*001) #*000))))
;; expressions are compiled and checked on model creation into two functions
;; - one for setting state and one for asserting outputs

(defclass asm-model(fsm)
  ((compiled-state-data :reader compiled-state-data))
  (:documentation "An Algorithmic State Machine Model"))

;; (defun compile-asm-expression(asm expr)
;;   "Return the two function forms - one for next state and one for output vector"
;;   (let* ((ipnames (mapcar #'name (subseq (inputs asm) 1)))
;;          (opnames (mapcar #'name (subseq (outputs asm) (length (state asm))))))
;;   (labels ((ip(ip) (or (position ip ipnames)
;;                      (error "Input ~A not defined." ip)))
;;            (op(op) (or (position op opnames)
;;                        (error "Output ~A not defined." op)))
;;            (doexpr(expr state-p)
;;              (typecase expr
;;                (bit-vector (when state-p (list expr)))
;;                (symbol (unless state-p
;;                          `((setf (aref output-vector ,(op exor)) 1))))
;;                (list
;;                 (if (eql (first expr) '?)
;;                     `((if (= 1 (bit input-vector ,(ip (second expr))))
;;                           ,@(doexpr (third expr) state-p)
;;                           ,@(doexpr (fourth expr) state-p)))
;;                     ,@(mapcar #'(lambda(expr) (doexpr expr state-p)) expr))))))
;;     (values
;;      `(lambda(input-vector)
;;         (let ((output-vector
;;               (make-array ,(length opnames)
;;                           :element-type 'bit
;;                           :initial-contents
;;                           ,(mapcar
;;                             #'(lambda(op)
;;                                 (if (member op (first expr)) 1 0))
;;                             opnames))))
;;           ,@(doexpr (third expr) nil)
;;           output-vector))
;;      `(lambda(input-vector) ,@(doexpr (third expr) t))))))

;; (defmethod initialize-instance :after ((asm asm) &key &allow-other-keys)
;;   (labels ((assert-low(c)
;;              (let ((n (symbol-name (name c))))
;;                (if (= (search "(L)"  :from-end t)
;;                       (length c)) 1 0)))
;;            (mask(c)
;;              (make-array (length c) :element-type 'bit
;;                          :initial-contents (mapcar #'assert-low c))))
;;   (setf (slot-value asm 'state-data)
;;         (mapcar #'(lambda(row)
;;                     (cons (car row)
;;                     (multiple-value-list (compile-asm-expression asm (rest row)))))
;;                 (state-data asm))
;;         (slot-value asm 'output-mask) (mask (subseq (outputs asm)
;;                                                     (length (state asm))))
;;         (slot-value asm 'input-mask) (mask (subseq (inputs asm) 1)))))

;; (defmethod next-state((asm asm) input-vector row)
;;   (funcall (third row) (bit-xor input-vector (slot-value asm 'input-mask))))

;; (defmethod output-vector((asm asm) input-vector row)
;;   (bit-xor
;;    (funcall (second row) (bit-xor input-vector (slot-value asm 'input-mask)))
;;    (slot-value asm 'output-mask)))

(defun cl-user::expr(os expr &rest args)
  "Print out a an expression as UTF-8 text"
  (declare (ignore args))
  (etypecase expr
    (symbol (write expr :stream os))
    (bit-vector (format os  "~{~A~}" (coerce expr 'list)))
    (list
     (let ((op (first expr)))
       (ecase op
         (or (format os "(~{~/expr/~^·~})" (rest expr)))
         (and (format os "(~{~/expr/~^×~})" (rest expr)))
         (xor (format os "(~{~/expr/~^xor~})" (rest expr)))
         (not (format os "~~~/expr/" (second expr)))
         (nor (cl-user::expr os (list 'not (cons 'or (rest expr)))))
         (nand (cl-user::expr os (list 'not (cons 'and (rest expr)))))
         (xnor (cl-user::expr os (list 'not (cons 'xor (rest expr))))))))))

(defmethod write-state-diagram((entity asm-model) (format (eql :dot))  &optional (stream *standard-output*))
  (let ((label 0) ;; numberical label for non state nodes
        (nodes))
    (labels ((edge(from to &optional options)
               (pushnew from nodes :test #'equal)
               (pushnew to nodes :test #'equal)
               (format stream
                       "~:[~A~;~/bv/~] -> ~:[~A~;~/bv/~] [~:[~;tailport=s,~]headport=n~@[,~A~]];~%"
                       (typep from 'bit-vector) from
                       (typep to 'bit-vector) to
                       (typep from 'bit-vector) options))
             (back-ref-p(e) (member e nodes :test #'equal))
             (expand-expr(expr from &optional options)
               (cond
                 ((typep expr 'bit-vector) ; move to next state
                  (edge from expr options))
                 ((eql (first expr) '?) ; conditional test
                  (unless (= (length expr) 4)
                    (error "~A should have 4 arguments" expr))
                  (let ((newlabel (incf label)))
                    (format stream "~D [label=\"~/expr/\",shape=diamond];~%"
                            newlabel (second expr))
                    (edge from newlabel options)
                    (let ((both-forward
                           (not (or (back-ref-p (third expr))
                                    (back-ref-p (fourth expr))))))
                      (expand-expr
                       (third expr) newlabel
                       (format nil "tailport=~:[s~;e~],taillabel=\"1\""
                               (or  (back-ref-p (third expr)) both-forward)))
                      (expand-expr
                       (fourth expr) newlabel
                       (format nil "tailport=~:[s~;w~],taillabel=\"0\""
                               (or  (back-ref-p (fourth expr)) both-forward))))))
                 (t ; conditional variables
                  (unless (= (length expr) 2)
                    (error "~A should only have 2 arguments" expr))
                  (let ((newlabel (incf label)))
                  (format stream "~D [label=\"~{~A~^\\n~}\",shape=box,style=rounded];~%"
                          label (first expr))
                  (edge from newlabel options)
                  (expand-expr (second expr) newlabel "tailport=s"))))))
  (write-line "digraph state_diagram {
rankdir=TB;
concentrate=true;
node[height=0.25];
Start [shape=none,label=\"Start\"];" stream)
  ;; write out state node data first
  (dolist(row (state-data entity))
    (format stream "~/bv/ [shape=record,label=\"~:*~/bv/|~@[~{~A~^\\n~}~]\"];~%"
            (first row) (second row)))
  ;; connect start to initial state
  (format stream "Start -> ~/bv/ [tailport=s,headport=n];~%" (initial-state-vector entity))
  ;; now write out the state data
  (dolist(row (state-data entity))
    (expand-expr (third row) (first row)))
  (write-line "}" stream))))
