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
(defvar *default-delay* 0)

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
    (when (gethash name *entities*)
      (error "Duplicate entity name ~A" name))
    (setf (gethash name *entities*) entity)))

(defmethod reset((entities hash-table))
  (maphash #'(lambda(k e)
               (declare (ignore k))
               (when (typep e 'source) (reset e)))
           entities)
  (maphash #'(lambda(k e)
                (declare (ignore k))
                (unless (typep e 'source) (reset e)))
           entities))

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
                   (make-instance 'output :entity entity :name name :signal-value v)))
             (call-next-method))))
  (:method((entity with-outputs) &key outputs &allow-other-keys) outputs))

(defmethod initialize-instance :after((entity with-outputs) &rest args &key &allow-other-keys)
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
  (:method((entity with-inputs) &key inputs &allow-other-keys) inputs))

(defmethod initialize-instance :after((entity with-inputs) &rest args &key &allow-other-keys)
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

(defgeneric calculate-output-signals(entity)
  (:documentation "Calculate and return new output signal vector
  from (current) inputs - if nil do not change outputs"))

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
           (delay (delay entity)))
      (when new-signals
        (let ((update-outputs
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
              (schedule delay update-outputs)))))))

(defgeneric connect(output input)
  (:documentation "Connect an output to an input")
  (:method :after(output (input input))
           (when (every #'connection (inputs (entity input)))
             (signals-changed (entity input))))
  (:method((output output) (input input))
    (when (connection input) (error "~A already connected" input))
    (pushnew (entity input) (connections output))
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
      (unless (find op (inputs (entity input)) :key #'connection)
        (setf (connections op) (delete (entity input) (connections op))))))))

(defun con-reader(is &optional char p)
  (declare (ignore char p))
  (do* ((names (read-delimited-list #\} is) (rest names))
        (entity (gethash (first names) *entities*)
                (gethash (first names) (components entity))))
       ((not (or (rest names) (typep entity 'logic-block)))
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

(defclass with-edge-detection()
  ((input-signal-vector :type bit-vector))
  (:documentation "A mixin to detect input signal edges"))

(defun input-signal-vector(entity)
  "Return 2 vectors - the input signal vector and a vector of changed inputs"
  (let ((new (signal-value (inputs entity))))
    (with-slots((old input-signal-vector)) entity
      (unless (slot-boundp entity 'input-signal-vector)
        (setf old new))
      (let ((changed (bit-xor new old)))
        (values (setf old new) changed)))))

(defun load-example(name &key (reset t))
  (when reset
    (clrhash *entities*)
    (reset *simulator*))
  (load (merge-pathnames
         (make-pathname :name name :type "lisp")
         #.(asdf:system-relative-pathname :logsim "/examples/"))
        :verbose nil :print nil)
  (format t "~%-- Example ~S loaded~%" name))
