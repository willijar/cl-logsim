;; LOGSIM Simulation Scheduler and event handling
;; Copyright 2007 Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See the LICENSE file provided or <http://www.gnu.org/licenses>

;;; Commentary:

;;

;;; Code:

(in-package :logsim)

(defvar *simulator* nil
 "The global simulator instance")

(defvar *reset-hooks* nil "List of hooks to call to reset simulation -
called after all entities created before running simulation")
(defvar *time-format* "~6,3f"  "Time output format control")

(defclass simulator ()
  ((entities :initform nil :accessor entities
             :documentation "List of all defined entities in this simulation")
   (simulation-time :type float :initform 0.0
	  :accessor simulation-time :initarg :start-time
	  :documentation "simulator virtual time")
   (halted :type boolean :initform t :accessor halted)
   (thread :initform nil :reader simulator-thread
	    :documentation "Thread running simulator")
   (event-queue
    :reader event-queue
    :initform
    (make-binary-heap
     :initial-size 1024
     :extend-size 1.4
     :element-type 'cons
     :key-fn #'car
     :comp-fn #'<))))

(defmethod print-object((simulator simulator) stream)
  (print-unreadable-object (simulator stream :type t :identity t)
     (format stream "time:~f~:[~; HALTED~] ~D pending events ~D entities"
	     (simulation-time simulator) (halted simulator)
       (alg:size (slot-value simulator 'event-queue))
       (length (entities simulator)))))

(defun schedule(delay function)
  (when (< delay 0)
    (error  "Attempt to schedule an event ~D seconds in the past"
             delay))
  (enqueue (cons (+ delay (simulation-time *simulator*)) function)
             (event-queue *simulator*))
    function)

(defmethod stop ((simulator simulator) &key abort)
  (setf (slot-value simulator 'halted) t)
  (when abort
    (when (slot-value simulator 'thread)
      (ignore-errors (kill-thread (slot-value simulator 'thread)))
      (setf (slot-value simulator 'thread) nil))))

(defmethod reset((simulator simulator))
  (stop simulator :abort t)
  (setf (simulation-time simulator) 0.0d0)
  (let ((q  (event-queue simulator)))
    (while (not (empty-p q)) (dequeue q)))
  (map 'nil
       #'(lambda(e) (when (typep e 'source) (reset e))) (entities simulator))
  (map 'nil
       #'(lambda(e) (unless (typep e 'source) (reset e))) (entities simulator)))

(defmethod start(simulator &key (granularity 10000)&allow-other-keys)
  "Execute the simulator returning the running
thread. granularity is the number of event to dispatch before
yielding the thread (default 10000). If granularity is nil all events
are dispatched in current thread"
  (flet((run(simulator)
          (setf (halted simulator) nil)
          (loop
           :with c = 1
           :with q =  (slot-value simulator 'event-queue)
           :while (not (or (empty-p q) (halted simulator)))
           :for event = (dequeue q)
           :do (progn
                 (setf (simulation-time simulator) (car event))
                 (funcall (cdr event)))
             :when (and granularity
                      (= (setf c (mod (1+ c) granularity)) 0))
           :do (yield-thread))
          (format t "~%-- Simulation halted at ~,4f~%" (simulation-time simulator))
          (setf (halted simulator) t)))
    (stop simulator :abort t)
    (if granularity
        (setf (slot-value simulator 'thread)
              (make-thread
               #'(lambda()
                   (run simulator)
                   (setf (slot-value simulator 'thread) nil))
               "LENS Simulator"))
        (funcall #'run simulator))))

(eval-when(:load-toplevel :execute)
  (unless *simulator* (setf *simulator* (make-instance 'simulator)))
  (pushnew *simulator* *reset-hooks*))