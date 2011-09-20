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

;; note we need event-counter to ensure that order is preserved
;;
(defclass simulator ()
  ((simulation-time :type float :initform 0.0
	  :accessor simulation-time :initarg :start-time
	  :documentation "simulator virtual time")
   (halted :type boolean :initform t :accessor halted)
   (event-counter :initform 0)
   (event-queue
    :reader event-queue
    :initform
    (make-binary-heap
     :initial-size 1024
     :extend-size 1.4
     :element-type 'cons
     :key-fn #'car
     :comp-fn #'(lambda(a b) (or (< (car a) (car b))
                                 (and (= (car a) (car b))
                                      (< (cdr a) (cdr b)))))))))

(defmethod print-object((simulator simulator) stream)
  (print-unreadable-object (simulator stream :type t :identity t)
     (format stream "time:~f~:[~; HALTED~] ~D pending events"
	     (simulation-time simulator) (halted simulator)
       (alg:size (slot-value simulator 'event-queue)))))

(defun schedule(delay function)
  (when (< delay 0)
    (error  "Attempt to schedule an event ~D seconds in the past"
             delay))
  (enqueue (cons (cons (+ delay (simulation-time *simulator*))
                       (incf (slot-value *simulator* 'event-counter)))
                 function)
             (event-queue *simulator*))
    function)

(defmethod stop ((simulator simulator) &key abort)
  (setf (slot-value simulator 'halted) t))

(defmethod reset((simulator simulator))
  (stop simulator :abort t)
  (setf (simulation-time simulator) 0.0d0)
  (let ((q  (event-queue simulator)))
    (while (not (empty-p q)) (dequeue q))))

(defmethod start(simulator &key (stop (+ (simulation-time simulator) 100))
                 step quiet &allow-other-keys)
  "Execute the simulator returning the running
thread. granularity is the number of event to dispatch before
yielding the thread (default 10000). If granularity is nil all events
are dispatched in current thread"
  (setf (halted simulator) nil)
  (loop
     :with q =  (slot-value simulator 'event-queue)
     :while (not (or (empty-p q)
                     (halted simulator)
                     (>= (simulation-time simulator) stop)))
     :for event = (dequeue q)
     :do (progn
           (setf (simulation-time simulator) (caar event))
           (when step (funcall step))
           (funcall (cdr event))))
  (unless quiet
    (format t "~%-- Simulation halted at ~,4f~%" (simulation-time simulator)))
  (setf (halted simulator) t))

(eval-when(:load-toplevel :execute)
  (unless *simulator* (setf *simulator* (make-instance 'simulator))))

(defun start-simulation(&key (stop 100) step)
  (start *simulator* :stop stop :step step))
(defun stop-simulation() (stop *simulator*))
