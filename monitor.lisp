;; Monitoring and tracing entites
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

(defclass monitor(entity with-inputs)
  ((action :initarg :action :reader action
           :documentation "Function to call when inputs have changed"))
  (:documentation "Entity to monitor signals"))

(defmethod inputs-changed((m monitor) &optional dummy)
  (declare (ignore dummy))
  (funcall (action m) (signal-value (inputs m))))

(defmethod connect((output output) (m monitor))
  (let ((input (make-instance 'input :entity m :name (name output))))
    (setf (slot-value m 'inputs)
          (concatenate 'vector (inputs m) (list input)))
    (connect output input))
  (reset m)
  (inputs-changed m))

(defmethod connect((outputs sequence) (monitor monitor))
  (map 'nil #'(lambda(input) (connect input monitor)) outputs))

(defclass trace-monitor(monitor)
  ((data :initform nil :type list :accessor data
         :documentation "List of trace data for this signal")))

(defmethod inputs-changed((tr trace-monitor) &optional changed-inputs)
  (declare (ignore changed-inputs))
  (setf (data tr)
        (cons (cons (simulation-time *simulator*) (signal-value (inputs tr)))
              (if (and (data tr)
                       (= (simulation-time *simulator*) (caar (data tr))))
                  (rest (data tr))
                  (data tr)))))

(defmethod reset((tr trace-monitor)) (setf (data tr) nil))

(defgeneric write-timing-diagram(trace format &optional stream)
  (:documentation "Write a timing diagram to stream"))

(defmethod write-timing-diagram((trace trace-monitor) (format (eql :tikz))
                                &optional (stream *standard-output*))
  (write-line "\\begin{tikzpicture}" stream)
  (let ((endtime (ceiling (car (first (data trace))))))
    (format stream "\\axis{~{-~D/~A~^,~}}{~D}~%"
            (mapcan #'(lambda(input n) (list n  (name input)))
                    (coerce (inputs trace) 'list)
                    (integer-sequence (length (inputs trace))))

            endtime)
    (let ((data (reverse (data trace))))
    (dotimes(i (length (inputs trace)))
      (flet ((b(sample) (aref (cdr sample) i)))
        (let ((last-b (b (first data))))
          (format stream "\\draw[trace] (t-~d)+(~,2f,~A)"
                  i (car (first data)) last-b)
          (dolist(sample (rest data))
            (let ((b (b sample))
                  (time (car sample)))
              (when (/= b last-b)
                (format stream "--+(~,2f,~A)--+(~,2f,~A)"
                        time last-b time b)
                (setf last-b b))))
          (format stream "--+(~,2f,~D);~%" endtime last-b))))))
  (write-line "\\end{tikzpicture}" stream))