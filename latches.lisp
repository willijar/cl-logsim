;; Latches and flip flop entities
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

(defclass latch(entity with-inputs with-outputs with-delay)
  ()
  (:documentation "An SR latch"))

(defmethod initialize-outputs((l latch) &key &allow-other-keys)
  '((Q . 0) (QBAR . 1)))

(defclass sr-latch(latch)
  ()
  (:documentation "An SR latch"))

(defmethod initialize-inputs((l sr-latch) &key &allow-other-keys)
  '(S R))

(defmethod calculate-output-signals((l sr-latch))
  (let ((iv (signal-value (inputs l))))
    (cond
      ((equal iv #*00) nil)
      ((equal iv #*10) #*10)
      ((equal iv #*01) #*01)
      ((equal iv #*11) #*00))))

(defclass d-latch(latch)
  ((control :initform 1 :accessor control :initarg :control
            :documentation "Control level"))
  (:documentation "A D type latch"))

(defmethod initialize-inputs((l sr-latch) &key &allow-other-keys)
  '(C D))

(defmethod calculate-output-signals((l d-latch))
  (let ((iv (signal-value (inputs l))))
    (when (= (aref iv 0) (control l))
      (if (zerop (aref iv 1)) #*01 #*10))))

(defclass sr-master-slave(sr-latch)
  ((control :initform 1 :reader control :initarg :control :type bit
            :documentation "Control level")
   (master-state :initform #*00 :type bit-vector)))

(defmethod initialize-inputs((l sr-master-slave) &key &allow-other-keys)
  '(S R C))

(defmethod calculate-output-signals((f sr-master-slave))
  (with-slots((ms master-state)) f
    (if (= (control f) (signal-value (aref (outputs f) 2)))
        (setf ms (call-next-method))
        ms)))

(defclass d-flip-flop(latch with-edge-detection)
  ((control :type bit :initform 1
            :initarg :control :initarg :edge :reader control
            :documentation "Edge triggering control - if 1 +ve, if 0 -ve")))

(defmethod initialize-inputs((flip-flop d-flip-flop)&key &allow-other-keys)
  '(CLK S R D))

(defmethod calculate-output-signals((flip-flop d-flip-flop))
  (multiple-value-bind(iv changed) (input-signal-vector flip-flop)
    (case
        (cond ((= 1 (aref iv 1)) 1)
              ((= 1 (aref iv 2)) 0)
              ((and (= 1 (aref changed 0)) ;clk edge
                    (= (aref iv 0) (control flip-flop)))
               (aref iv 3)))
      (0 #*01)
      (1 #*10))))

(defclass jk-flip-flop(latch with-edge-detection)
  ((control :initform 1 :initarg :control :initarg :edge :reader control
            :documentation "Edge triggering control - if 1 +ve, if 0 -ve")))

(defmethod initialize-inputs((flip-flop jk-flip-flop)&key &allow-other-keys)
  '(CLK S R J K))

(defmethod calculate-output-signals((flip-flop jk-flip-flop))
  (multiple-value-bind(iv changed) (input-signal-vector flip-flop)

    (case
        (cond ((= 1 (aref iv 1)) 1) ; set
              ((= 1 (aref iv 2)) 0) ; reset
              ((= 0 (aref changed 0))) ; not clk edge - finished
              ((= (aref iv 3) (aref iv 4)) ; J=K
               (when (= (aref iv 3) 1)
                 (bit-not (signal-value (outputs flip-flop)))))
              ((= (aref iv 3) 1) 1) ; J=1
              (0)) ; K=1
      (0 #*01)
      (1 #*10))))




