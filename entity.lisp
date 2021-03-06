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

(defvar *entities* (make-hash-table)  "Hash of all entities")

(defconstant high 1 "High value boolean")
(defconstant low 0 "High value boolean")
(defvar *default-delay* 0.05)

(defclass entity()
  ((lastid :allocation :class :initform 0)
   (name :initarg :name :type symbol :accessor name))
  (:documentation "A general simulation entity"))

(defmethod print-object((entity entity) stream)
  (if *print-readably*
      (format stream "#{~A}" (name entity))
      (print-unreadable-object(entity stream :type t)
        (write (name entity) :stream stream))))

(defmethod initialize-instance :after ((entity entity) &key &allow-other-keys)
  (with-slots(name lastid) entity
    (incf lastid)
    (unless (slot-boundp entity 'name)
      (setf name
            (intern (format nil "~A~3,'0D" (class-name (class-of entity))
                            lastid))))
    (restart-case
        (when (gethash name *entities*)
          (error "Duplicate entity name ~A" name))
      (continue() :report "Replace previous definition"))
    (setf (gethash name *entities*) entity)))

(defclass connection()
  ((entity :type entity :initarg :entity :reader entity
           :documentation "Entity with this connection")
   (name :type symbol :initarg :name :initform 'op :reader name))
  (:documentation "A connection to an entity"))

(defgeneric signal-value(signal)
  (:documentation "Return the binary signal value of something")
  (:method((s integer)) s)
  (:method((s null)) 'Unconnected))

(defmethod print-object((c connection) stream)
  (if *print-readably*
      (format stream "#{~A ~A}" (name (entity c)) (name c))
      (print-unreadable-object(c stream :type t)
        (format stream "~A ~A=~A" (name (entity c)) (name c) (signal-value c)))))

(defclass output(connection)
  ((connections :type list :accessor connections :initform nil
                :documentation "List of inputs this is connected to")
   (signal-value :initarg :signal-value
                 :type bit :accessor signal-value :initform 0)))

(defclass input(connection)
  ((connection :type output :accessor connection :initform nil
               :documentation "The output connected to this input")))

(defclass with-outputs()
  ((outputs :type (vector output *) :reader outputs))
  (:documentation "Mixin for entity with one or more outputs"))

(defgeneric initialize-outputs(entity &key &allow-other-keys)
  (:documentation "Main method should return a list of cons of names and intitial values for outputs of an entity")
  (:method :around ((entity with-outputs) &key &allow-other-keys)
      (setf (slot-value entity 'outputs)
            (map 'vector
                 #'(lambda(init)
                 (multiple-value-bind(name v)
                     (if (consp init)
                         (values (car init) (cdr init))
                         (values init 0))
                   (make-instance 'output :entity entity
                                  :name name :signal-value v)))
             (call-next-method))))
  (:method((entity with-outputs) &key outputs &allow-other-keys) outputs))

(defmethod initialize-instance :after((entity with-outputs)
                                      &rest args &key &allow-other-keys)
  (apply #'initialize-outputs entity args))

(defclass with-inputs()
  ((inputs :type (vector input *) :reader inputs))
  (:documentation "mixin for an entity with one or more inputs"))

(defgeneric initialize-inputs(entity &key &allow-other-keys)
  (:documentation "Main method should return a list of cons of names and intitial values for inputs of an entity")
  (:method :around ((entity with-inputs) &key &allow-other-keys)
    (setf (slot-value entity 'inputs)
        (map 'vector
             #'(lambda(name)
                 (make-instance 'input :entity entity :name name))
             (call-next-method))))
  (:method((entity with-inputs) &key inputs &allow-other-keys)
    inputs))

(defmethod initialize-instance :after((entity with-inputs)
                                      &rest args &key &allow-other-keys)
  (apply #'initialize-inputs entity args))

(defmethod signal-value((ip input))
  (signal-value (connection ip)))

(defmethod signal-value((v sequence))
  (map 'bit-vector #'signal-value v))

(defmethod (setf signal-value)((b vector) (entity sequence))
  (map 'bit-vector #'(setf signal-value) b entity))

(defmethod (setf signal-value)((b integer) (input input))
  (connect b input))

(defclass connector(input output)
  ()
  (:documentation "A connector (an alias for an input or output)"))

(defmethod signal-value((c connector))
  (if (connection c)
      (signal-value (connection c))
      (slot-value c 'signal-value)))

(defmethod (setf signal-value)(v (c connector))
  (declare (ignore v))
  (error "Attempt to set a signal value of ~A" c))

(defgeneric calculate-output-signals(entity &optional changed-inputs)
  (:documentation "Calculate and return new output signal vector
  from (current) inputs. if nil do not change outputs"))

(defgeneric delay(entity)
  (:documentation "Gate delay for entity")
  (:method(entity) 0))

(defclass with-delay(with-outputs)
  ((delay :initarg :delay
          :type float :initform *default-delay* :accessor delay
          :documentation "Gate delay"))
  (:documentation "An entity with a fixed delay between inputs
  changing and outputs being updated"))

(defmethod inputs-changed((fn (eql :all)) &optional to-alert)
  "Given a list of inputs to alert that their signals have changed -
collate them and notify the entities."
  (while to-alert
      (let ((changed-inputs (list (first to-alert)))
            (entity (entity (first to-alert))))
        (setf to-alert
              (mapcan
               #'(lambda(input)
                   (if (eql (entity input) entity)
                           (progn
                             (push input changed-inputs)
                             nil)
                           (list input)))
               (rest to-alert)))
        (inputs-changed entity changed-inputs))))

(defgeneric change-outputs(opvec entity)
  (:documentation "Change the outputs of entity to op informing all
  connected entities - pass changes to connected objects (after delay
  if necessary")
  (:method(new-signals (outputs sequence))
    (when new-signals
      (let* ((old-signals (signal-value outputs))
             (to-alert
              (apply #'append
                     (map 'list
                          #'(lambda(oldv newv output)
                              (unless (equal oldv newv)
                                (connections output)))
                          old-signals new-signals outputs))))
        (setf (signal-value outputs) new-signals)
        (inputs-changed :all to-alert))))
  (:method(new-signals (entity with-outputs))
    (change-outputs new-signals (outputs entity))))

(defgeneric inputs-changed(entity &optional changed-inputs)
  (:documentation "Inform an entity that its inputs have changed -
  optionally notifying which inputs")
  (:method((entity with-outputs) &optional changed-inputs)
        (schedule
         (delay entity)
         (let ((opvec (calculate-output-signals entity changed-inputs)))
           #'(lambda() (change-outputs opvec entity))))))

(defgeneric connect(output input)
  (:documentation "Connect an output to an input")
  (:method :after(output (input input))
     (when (every #'connection (inputs (entity input)))
       (inputs-changed (entity input) (inputs (entity input)))))
  (:method((output output) (input input))
    (restart-case
        (when (connection input)
          (error "Unable to  connect ~A to ~A. ~A already connected."
                 output input (connection input)))
      (abort()
          :report "Abort this connection"
          (return-from connect))
      (replace()
          :report "Replace previous connection"))
    (pushnew input (connections output))
    (setf (connection input) output))
  (:method ((output integer) (input input))
    (setf (connection input) output))
  (:method (output (inputs sequence))
    (map 'nil #'(lambda(input) (connect output input)) inputs))
  (:method ((outputs sequence) (inputs sequence))
    (map 'nil #'connect outputs inputs)))

(defgeneric disconnect(input)
  (:documentation "Disconnect an input")
  (:method((input input))
    (let ((op (connection input)))
      (setf (connection input) nil)
      (when (typep op 'output)
        (setf (connections op) (delete input (connections op)))))))

(defclass with-clk(with-inputs)
  ((control :initform 1 :type bit :initarg :control
            :initarg :edge :reader control
            :documentation "Clock edge transition control - +ve if 1")
   (clk-input :type input :reader clk-input
              :documentation "The clock input connection")))

(defmethod initialize-instance :after ((entity with-clk) &key (clk-input 'CLK))
  (setf (slot-value entity 'clk-input)
        (etypecase clk-input
          (symbol
           (or (find clk-input (inputs entity) :key 'name)
               (error "~A is not a valid clock for ~A" clk-input entity)))
          (input clk-input)
          (number (aref (inputs entity) clk-input)))))

(defun clk-edge-p(entity changed-inputs)
  (let ((clk (clk-input entity)))
    (and (= (signal-value clk) (control entity))
         (member clk changed-inputs))))

(defun load-example(name &key (reset t) (quiet t))
  (when reset
    (clrhash *entities*)
    (reset *simulator*))
  (load (if (pathnamep name) name
            (merge-pathnames
             (make-pathname :name name :type "lisp")
             #.(asdf:system-relative-pathname :logsim "examples/")))
        :verbose nil :print nil)
  (unless quiet (format *trace-output* "~%-- Example ~S loaded~%" name)))
