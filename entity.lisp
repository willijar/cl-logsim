;; <description>
;; Copyright (C) 2009 Dr. John A.R. Williams

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
   (name :type symbol :initarg :name :initform 'op))
  (:documentation "A connection to an entity"))

(defgeneric signal-value(signal)
  (:documentation "Return the binary signal value of en entity")
  (:method((s integer)) s))

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
  (if (slot-boundp ip 'connection)
      (signal-value (connection ip))
      'unconnected))

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
                        (mapcan
                             #'(lambda(oldv newv output)
                                 (unless (= oldv newv) (copy-list (connections output))))
                             old-signals new-signals (outputs entity))))
                  (setf (signal-value (outputs entity)) new-signals)
                  (map 'nil #'signals-changed (delete-duplicates to-alert))))))
      (if (zerop delay)
          (funcall update-outputs)
          (schedule delay update-outputs)))))

(defgeneric connect(output input)
  (:documentation "Connect an output to an input")
  (:method((output output) (input input))
    (when (connection input) (error "~A already connected" input))
    (pushnew (entity input) (connections output))
    (setf (connection input) output)))

(defgeneric disconnect(input)
  (:documentation "Disconnect an input")
  (:method((input input))
    (let ((op (connection input)))
      (setf (connection input) nil)
      (unless (find op (inputs (entity input)) :key #'connection)
        (setf (connections op) (delete (entity input) (connections op)))))))

(defclass source(entity with-outputs)
  ((signal-sequence :type vector :reader signal-sequence
                    :documentation "Sequence of values for this
signal.  These are specified as a duration followed by the signal
values. For non-periodic signals the last time period is ignored.")
   (signal-index :type fixnum :initform 0 :accessor signal-index
                   :documentation "Current position in sequence")
   (periodic :initarg :periodic :type boolean :initform nil :reader periodic)))

(defmethod initialize-instance :after ((s source)
                                       &key sequence &allow-other-keys)
  (let ((n (length (outputs s))))
    (setf (slot-value s 'signal-sequence)
          (map 'vector
               #'(lambda(s)
                   (cons
                    (car s)
                    (if (and (= n 1) (atom (cdr s)))
                        (make-array 1 :element-type 'bit
                                    :initial-element (cdr s))
                        (coerce (cdr s) `(bit-vector ,n)))))
               sequence))))

(defmethod calculate-output-signals((s source))
  (cdr (aref (signal-sequence s) (signal-index s))))

(defmethod signals-changed((s source))
  (with-slots((idx signal-index) (seq signal-sequence)) s
    (incf idx)
    (when (>= idx (length seq))
      (if (periodic s)
          (setf idx 0)
          (return-from signals-changed)))
    ;; inform connected entities and reschedule if necessary
    (call-next-method)
    (schedule (car (aref seq idx)) #'(lambda() (signals-changed s)))))

(defmethod reset((s source))
  (setf (signal-index s) 0)
  (let ((v (aref (signal-sequence s) 0)))
    (setf (signal-value (outputs s)) v)
    (schedule (car v) #'(lambda() (signals-changed s)))))

(defun make-clock(&key (period 1) (name 'CLK))
  "Clock is just a periodic signal with one output"
  (make-instance 'source
                 :name name
                 :sequence `((,(/ period 2) . 0) (,(/ period 2) . 1))
                 :periodic t))

(defclass logic(entity with-inputs with-outputs)
  ((delay :type float :initform 0.05 :reader delay
          :documentation "Gate delay"))
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

(defclass monitor(entity with-inputs)
  ((input-initialization :initarg :inputs :reader input-initialization
                         :initform #(IP))
   (action :initarg :action :reader action
           :documentation "Function to call when inputs have changed"))
  (:documentation "Entity to monitor signals"))

(defmethod signals-changed((m monitor))
  (funcall (action m) (signal-value (inputs m))))

(defclass trace-monitor(monitor)
  ((data :initform nil :type list :accessor data
         :documentation "List of trace data for this signal")))

(defmethod initialize-instance :after ((tr trace-monitor) &key &allow-other-keys)
  (setf (slot-value tr 'action)
        #'(lambda(bits)
            (push (cons (simulation-time *simulator*) bits) (data tr)))))

(defmethod reset((tr trace-monitor)) (setf (data tr) nil))

(defun con-reader(is &optional char p)
  (declare (ignore char p))
  (let* ((names (read-delimited-list #\} is))
         (entity (find (first names) (entities *simulator*)
                       :key #'name))
         (pin (second names)))
    (unless entity  (error "No entity ~A defined" entity))
    (unless pin (return-from con-reader entity))
    (cond
      ((when (typep entity 'with-inputs)
         (find pin (inputs entity)) :key #'name))
      ((when (typep entity 'with-outputs)
         (find pin (outputs entity) :key #'name)))
      (t (error "No connection ~A on ~A" pin entity)))))

(set-dispatch-macro-character #\# #\{ #'con-reader)
