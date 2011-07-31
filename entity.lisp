;; <description>
;; Copyright (C) 2011 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;;; Copying:

;; This file is part of

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

(defconstant high 1 "High value boolean")
(defconstant low 0 "High value boolean")
(defvar *default-delay* 0.05)

(defclass entity()
  ((lastid :allocation :class :initform 0)
   (id :type integer :reader id)
   (name :initarg :name :type symbol :accessor name))
  (:documentation "A general simulation entity"))

(defmethod print-object((entity entity) stream)
  (if *print-readably*
      (format stream "#{~A}" (name entity))
      (print-unreadable-object(entity stream :type t)
        (write (name entity) :stream stream))))

(defmethod initialize-instance :after ((e entity) &key &allow-other-keys)
  (setf (slot-value e 'id) (incf (slot-value e 'lastid)))
  (unless (slot-boundp e 'name)
    (setf (slot-value e 'name)
          (intern (format nil "~A~3,'0D" (class-name (class-of e)) (id e)))))
  (push e (entities *simulator*)))

(defclass connection()
  ((entity :type entity :initarg :entity :reader entity
           :documentation "Entity with this connection")
   (name :type symbol :initarg :name :initform 'op :reader name))
  (:documentation "A connection to an entity"))

(defgeneric signal-value(signal)
  (:documentation "Return the binary signal value of en entity")
  (:method((s integer)) s)
  (:method((s null)) 'Unconnected))

(defmethod print-object((c connection) stream)
  (if *print-readably*
      (format stream "#{~A ~A}" (name (entity c)) (name c))
      (print-unreadable-object(c stream :type t)
        (format stream "~A ~A=~A" (name (entity c)) (name c) (signal-value c)))))

(defclass output(connection)
  ((connections :type list :accessor connections :initform nil
                :documentation "List of input this is connected to")
   (signal-value :initarg :signal-value
                 :type bit :accessor signal-value :initform 0)))

(defclass input(connection)
  ((connection :type output :accessor connection :initform nil
               :documentation "The output connected to this input")))

(defclass with-outputs()
  ((outputs :type (vector output *) :reader outputs))
  (:documentation "Mixin for entity with one or more outputs"))

(defgeneric output-initialization(entity)
  (:documentation "Return a list of cons of names and intitial values for outputs of an entity")
  (:method((entity with-outputs)) '(OP)))

(defmethod initialize-instance :after((entity with-outputs) &key &allow-other-keys)
  (setf (slot-value entity 'outputs)
        (map 'vector
             #'(lambda(init)
                 (multiple-value-bind(name v)
                     (if (consp init)
                         (values (car init) (cdr init))
                         (values init 0))
                   (make-instance 'output :entity entity :name name :signal-value v)))
             (output-initialization entity))))

(defmethod reset((entity with-outputs))
  (setf (signal-value (outputs entity))
        (map 'bit-vector
             #'(lambda(init)
                 (if (consp init) (cdr init) 0))
             (output-initialization entity))))

(defclass with-inputs()
  ((inputs :type (vector input *) :reader inputs))
  (:documentation "mixin for an entity with one or more inputs"))

(defgeneric input-initialization(entity)
  (:documentation "Return a list of cons of names and intitial values for outputs of an entity")
  (:method((entity with-outputs)) '(IP)))

(defmethod initialize-instance :after((entity with-inputs) &key &allow-other-keys)
  (setf (slot-value entity 'inputs)
        (map 'vector
             #'(lambda(name)
                 (make-instance 'input :entity entity :name name))
             (input-initialization entity))))

(defmethod signal-value((ip input))
  (signal-value (connection ip)))

(defmethod signal-value((v vector))
  (map 'bit-vector #'signal-value v))

(defmethod (setf signal-value)((entity vector) (b vector))
  (map 'bit-vector #'(setf signal-value) entity b))

(defgeneric calculate-output-signals(entity)
  (:documentation "Calculate and return new output signal vector
  from (current) inputs"))

(defgeneric delay(entity)
  (:documentation "Return the delay in updating outputs after an input
  has changed for entity")
  (:method(entity) 0))

(defclass with-delay()
  ((delay :initarg :delay :type float :initform *default-delay* :accessor delay
          :documentation "Gate delay"))
  (:documentation "An entity with a fixed delay between inputs
  changing and outputs being updated"))

(defgeneric signals-changed(entity)
  (:documentation "Inform a destination entity that an input has
  changed - the corresponding output being on src")
  (:method((entity with-outputs))
    "For an entity with outputs calculate new oututs and schedule update"
    (let* ((new-signals (calculate-output-signals entity))
           (delay (delay entity))
           (update-outputs
            #'(lambda()
                (let* ((old-signals (signal-value (outputs entity)))
                       (to-alert
                        (apply #'nconc
                               (map 'list
                                    #'(lambda(oldv newv output)
                                        (unless (= oldv newv)
                                   (copy-list (connections output))))
                             old-signals new-signals (outputs entity)))))
                  (setf (signal-value (outputs entity)) new-signals)
                  (map 'nil #'signals-changed (delete-duplicates to-alert))))))
      (if (zerop delay)
          (funcall update-outputs)
          (schedule delay update-outputs)))))

(defgeneric connect(output input)
  (:documentation "Connect an output to an input")
  (:method :before(output (input input))
           (when (connection input) (error "~A already connected" input)))
  (:method :after(output (input input))
           (when (every #'connection (inputs (entity input)))
             (signals-changed (entity input))))
  (:method((output output) (input input))
    (pushnew (entity input) (connections output))
    (setf (connection input) output))
  (:method((output integer) (input input))
    (setf (connection input) output)))

(defgeneric disconnect(input)
  (:documentation "Disconnect an input")
  (:method((input input))
    (let ((op (connection input)))
      (setf (connection input) nil)
      (when (typep op 'output)
      (unless (find op (inputs (entity input)) :key #'connection)
        (setf (connections op) (delete (entity input) (connections op))))))))

(defun con-reader(is &optional char p)
  (declare (ignore char p))
  (let* ((names (read-delimited-list #\} is))
         (entity (find (first names) (entities *simulator*)
                       :key #'name))
         (pin (second names)))
    (unless entity  (error "No entity ~A defined" entity))
    (unless pin (return-from con-reader entity))
    (or
      (when (typep entity 'with-inputs)
        (find pin (inputs entity) :key #'name))
      (when (typep entity 'with-outputs)
        (find pin (outputs entity) :key #'name))
      (error "No connection ~A on ~A" pin entity))))

(set-dispatch-macro-character #\# #\{ #'con-reader)
