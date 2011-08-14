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

(defmethod signals-changed((m monitor))
  (funcall (action m) (signal-value (inputs m))))

(defmethod connect((output output) (m monitor))
  (let ((input (make-instance 'input :entity m :name (name output))))
    (setf (slot-value m 'inputs)
          (concatenate 'vector (inputs m)
                       (list input)))
    (connect output input))
  (reset m)
  (signals-changed m))

(defmethod connect((outputs sequence) (monitor monitor))
  (map 'nil #'(lambda(input) (connect input monitor)) outputs))

(defclass trace-monitor(monitor)
  ((data :initform nil :type list :accessor data
         :documentation "List of trace data for this signal")))

(defmethod initialize-instance :after ((tr trace-monitor) &key &allow-other-keys)
  (setf (slot-value tr 'action)
        #'(lambda(bits)
            (setf (data tr)
                  (cons (cons (simulation-time *simulator*) bits)
                        (if (and (data tr)
                                 (= (simulation-time *simulator*)
                                    (caar (data tr))))
                            (rest (data tr))
                            (data tr)))))))

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


;; (defgeneric truth-table(entity)
;;   (:documentation "Return the truth table for entity"))

;; (defmethod truth-table(entity)
;;   (labels ((free-inputs(x)
;;              (etypecase x
;;                (input (unless (connection x) (list x)))
;;                (with-inputs (free-inputs (coerce (inputs x) 'list)))
;;                (list (mapcan #'free-inputs x))))
;;            (free-outputs(x)
;;              (etypecase x
;;                (output (when (or (not (connections x))
;;                                  (and (listp entity)
;;                                       (set-difference (connections x) entity)))
;;                          (list x)))
;;                (with-outputs (free-outputs (coerce (outputs x) 'list)))
;;                (list (mapcan #'free-outputs x)))))
;;     (let* ((inputs (free-inputs entity))
;;            (outputs (free-outputs entity))
;;            (n (length inputs))
;;            (op
;;             (map 'vector
;;             #'(lambda(input-values)
;;                 (setf (signal-value inputs) input-values)
;;                 (cons input-values (signal-value outputs)))
;;             (mapcar #'(lambda(i) (integer-to-bit-vector i n))
;;                     (integer-sequence (ash 1 n))))))
;;       (map 'nil #'disconnect inputs)
;;       (values op
;;               (cons inputs outputs)))))

;; (defgeneric write-truth-table(trace format &optional stream)
;;   (:documentation "Write a timing diagram to stream"))

;; (defmethod write-truth-table(entity (format (eql :tikz))
;;                              &optional (stream *standard-output*))
;;   (multiple-value-bind(data columns) (truth-table entity)
;;     (format stream "\\begin{tabular}{~{~*c~}|~{~*c~}}~%" (car columns) (cdr columns))
;;     (format stream "\\multicolumn{~D}{c|}{Inputs} & \\multicolumn{~D}{c}{Outputs} \\\\~%" (length (car columns)) (length (cdr columns)))
;;     (format stream "~{~A &~}~{~A~^ &~}\\\\\\hline~%"
;;             (mapcar #'name (car columns)) (mapcar #'name (cdr columns)))
;;     (loop :for row :across data
;;        :do (format stream "~{~A &~}~{~A~^&~}\\\\~%"
;;                    (coerce (car row) 'list)
;;                    (coerce (cdr row) 'list)))
;;     (write-line "\\end{tabular}" stream)))