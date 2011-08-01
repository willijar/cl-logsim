;; Block entity
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

(defvar *block-definitions* (make-hash-table) "Block Definitions")

(defmacro define-logic-block(name (&rest args) &body body)
  (setf (gethash name *block-definitions*)
        `(lambda(,@args) ,@body)))

(defclass logic-block(entity with-inputs with-outputs)
  ((block-type :type symbol :initarg :type :reader block-type
               :documentation "Block type definition")
   (components :initform (make-hash-table) :reader components
               :documentation "components in this entity"))
  (:documentation "A logic block"))

(defmethod initialize-instance :before ((b logic-block) &key type &allow-other-keys)
  (unless (gethash type *block-definitions*)
    (error "No logic block ~A defined." type)))

(defmethod initialize-inputs  ((b logic-block) &rest args &key type &allow-other-keys)
  (getf (funcall (gethash type *block-definitions*) args) :inputs))

(defmethod initialize-outputs ((b logic-block) &rest args &key type &allow-other-keys)
  (getf (funcall (gethash type *block-definitions*) args) :outputs))

(defmethod initialize-instance :after ((b logic-block) &rest args &key type &allow-other-keys)
  ;; all inputs and outputs should be connectors
  (map 'nil #'(lambda(n) (change-class n 'connector))
       (append (inputs b) (outputs b)))
  (let ((def (funcall (gethash type *block-definitions*) args))
        (components (components b)))
    ;; create and add components to this block
    (dolist(def (getf def :components))
      (let* ((type (second def))
             (local-name (first def))
             (init (cdr def))
             (global-name (intern (format nil "~A.~A" (name b) local-name))))
        (setf (gethash local-name components)
              (apply #'make-instance type `(:name ,global-name ,@init)))))
    ;; connect up components and connectors
    (flet ((connection(def dir)
             (etypecase def
               (integer def)
               (symbol ; must be block connection
                (find def (funcall (ecase dir (:from #'inputs) (:to #'outputs))
                                   b)
                      :key #'name))
               (list ; must be component connection
                (find (cdr def)
                      (funcall (ecase dir (:from #'outputs) (:to #'inputs))
                               (gethash (car def) components))
                      :key #'name)))))
    (dolist(def (getf def :connections))
      (connect (connection (car def) :from)
               (connection (cdr def) :to))))
  ;; check connectivity
    (maphash
     #'(lambda(name component)
         (let ((n (find nil (inputs component) :key #'connection)))
           (when n (error "Input ~A of component ~A in ~A block ~A not connected"
                          n name type (name b))))
         (let ((n (find nil (outputs component) :key #'connections)))
           (when n (error "Output ~A of component ~A in ~A block ~A not connected"
                          n name type (name b)))))
     components)
    (let ((unconnected (find nil (inputs b) :key #'connections)))
      (when unconnected (error "Block Input ~A unconnected" unconnected)))
    (let ((unconnected (find nil (outputs b) :key #'connection)))
      (when unconnected (error "Block Output ~A unconnected" unconnected)))))

