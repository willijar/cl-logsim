;; Block entity
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

(defclass logic-block(entity with-inputs with-outputs)
  ((definition :initarg :definition :initarg :type :reader definition
               :documentation "Block type definition")
   (components :initform (make-hash-table) :reader components
               :documentation "locally named components in this entity"))
  (:documentation "A logic block"))

(defmethod initialize-instance :after ((b logic-block) &key &allow-other-keys)
  ;; all inputs and outputs should be connectors
  (map 'nil #'(lambda(n) (change-class n 'connector))
       (concatenate 'list (inputs b) (outputs b))))

(defvar *block-definitions* (make-hash-table)
  "Global registry of named Block Definitions")

(defmacro define-logic-block(name (&rest args) &body body)
  (setf (gethash name *block-definitions*)
        `(lambda(,@args) ,@body)))

(defun get-block-definition(definition args)
  (etypecase definition
    (symbol (funcall (or (gethash definition *block-definitions*)
                         (error "No defined bloc type ~A" definition))
                     args))
    (list definition)
    (function (funcall definition args))))

(defmethod inputs-changed((b logic-block) &optional changed-inputs)
  (inputs-changed :all
                  (apply #'append (map 'list #'connections changed-inputs))))

(defvar *env* (make-hash-table)
  "Name space environment for logic block construction. Maps names to either
an output to be resolved to a given name or a list of inputs which
need to be connected to that symbol once it is resolved.")

(defun resolve-output(output name)
  (let ((r (gethash name *env*)))
             (when (typep r 'output) (error "Duplicate output name ~A" name))
             (map 'nil #'(lambda(input) (connect output input)) r)
             (setf (gethash name *env*) output)))

(defun resolve-input(input expr)
  "Returns list of gates created"
  (etypecase expr
    (symbol (let ((r (gethash expr *env*)))
              (if (typep r 'output)
                  (connect r input)
                  (setf (gethash expr *env*) (cons input r)))
            nil))
    (output (connect expr input) nil)
    (bit (connect expr input) nil)
    (list
     ;; check for output binding operator <=
     (let* ((bind (eql (car expr) '<=))
            (gates (build-logic (if bind (third expr) expr)))
            (gate (first gates)))
       (connect
        (if (and bind (symbolp (second expr)))
            (find (second expr) (outputs gate) :key #'name)
            (aref (outputs gate) (if bind (second expr) 0)))
        input)
       gates))))

(defun build-logic(expr)
  "Construct a logic block using gates - returns a list of new gates
in topoligical order. "
  (multiple-value-bind(type init-args args)
      (etypecase (car expr)
        (symbol (values (car expr) nil (cdr expr)))
        (list (values (caar expr) (cdar expr) (cdr expr))))
    (case type
      (=>
       ;; variable assignment
       (unless (= (length args) 2)
         (error "Invalid Expression ~A 2 arguments expected" expr))
       (let ((gates (build-logic (second args))))
         (resolve-output (aref (outputs (first gates)) 0) (first args))
         gates))
      (not
       ;; not gate
       (unless (= (length args) 1)
         (error "Invalid Expression ~A 1 argument expected" expr))
       (let ((gate (apply #'make-instance 'not-gate init-args)))
         (cons gate
               (resolve-input (aref (inputs gate) 0) (first args)))))
      ((or and nor nand xor)
       ;; other common gates
       (unless (>= (length args) 2)
         (error "Invalid Expression ~A >1 argument expected" expr))
       (let* ((inputs-arg)
              (new-args nil))
         ;; we convert nots into inverting inputs
         (dolist(expr args)
           (if (and (listp expr) (eql (car expr) 'not))
               (progn
                 (push #\i inputs-arg)
                 (push (second expr) new-args))
               (progn
                 (push #\n inputs-arg)
                 (push expr new-args))))
         (let* ((gate
                 (apply #'make-instance
                        (intern (format nil "~A-GATE" (car expr)))
                        `(:inputs ,(coerce (reverse inputs-arg) 'string)
                                  ,@init-args))))
           (cons
            gate
            (mapcan #'resolve-input
                    (coerce (inputs gate) 'list)
                    (reverse new-args))))))
      (t ;; all other logic units
       (let ((gate
              (if (eql type 'logic-block)
                  (build-logic-block
                   (getf (cddr init-args) :name)
                   (get-block-definition (second init-args) (cddr init-args)))
                  (apply #'make-instance type init-args))))
         (cons gate
               (loop
                  :for r :on args :by #'cddr
                  :for name = (car r)
                  :for expr = (cadr r)
                  :for input = (find name (inputs gate) :key #'name)
                  :for output = (find name (outputs gate) :key #'name)
                  :when input :nconc (resolve-input input expr)
                  :when output :do (resolve-output output expr))))))))

(defun build-logic-block(name expr)
  "Build a logic block with one output from expression. Undefined
variables will be mapped onto inputs on the block. "
  (let* ((*env* (make-hash-table))
         outputs-names
         inputs-names
         (gates
          (mapcan
           #'(lambda(expr)
               (if(eql (car expr) '=>)
                  (progn
                    (push (second expr) outputs-names)
                    (build-logic expr))
                  (build-logic expr)))
           expr)))
    (maphash ;; collect list of unresolved inputs as input names
     #'(lambda(name v)
         (when (and (listp v) (not (member name outputs-names))
           (push name inputs-names))))
     *env*)
    (let ((gate
           (make-instance
            'logic-block
            :name name
            :inputs (reverse inputs-names)
            :outputs (reverse outputs-names)
            :definition expr)))
      ;; connect up the inputs and outputs to components
      (maphash
       #'(lambda(name v)
           (etypecase v
             (list
              (map 'nil
                   #'(lambda(input)
                       (connect (find name (inputs gate) :key #'name) input))
                   v))
             (output
              (connect v (find name (outputs gate) :key #'name)))))
       *env*)
      ;; register gates in block
      (dolist(igate gates)
        (setf (gethash (name igate) (components gate)) igate))
      gate)))

(defun con-reader(is &optional char p)
  (declare (ignore char p))
  (do* ((names (read-delimited-list #\} is) (rest names))
        (entity (gethash (first names) *entities*)
                (gethash (first names) (components entity))))
       ((not (cddr names))
        (when entity
          (if (rest names)
              (let ((pin (second names)))
                (or
                 (when (typep entity 'with-inputs)
                   (find pin (inputs entity) :key #'name))
                 (when (typep entity 'with-outputs)
                   (find pin (outputs entity) :key #'name))))
              entity)))))

(set-dispatch-macro-character #\# #\{ #'con-reader)

(defmethod truth-table(entity)
  (reset *simulator*)
  (values
   (let ((n (length (inputs entity))))
    (map 'list
         #'(lambda(i)
             (let ((iv (integer-to-bit-vector i n)))
               (setf (signal-value (inputs entity)) iv)
               (start *simulator* :quiet t)
               (cons iv (signal-value (outputs entity)))))
         (integer-sequence (ash 1 n))))
   (cons (map 'vector #'name (inputs entity))
         (map 'vector #'name (outputs entity)))))

(defmethod truth-table((expr list))
  (truth-table (build-logic-block (gensym "truth-table") expr)))