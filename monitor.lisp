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

