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

(defvar *block-definitions* (make-hash-table)
  "Global registry of named Block Definitions")

(defun get-block-definition(definition args)
  (etypecase definition
    (symbol (funcall (or (gethash definition *block-definitions*)
                         (error "No defined bloc type ~A" definition))
                     args))
    (list definition)
    (function (funcall definition args))))

(defmacro define-logic-block(name (&rest args) &body body)
  (setf (gethash name *block-definitions*)
        `(lambda(,@args) ,@body)))

(defclass logic-block(entity with-inputs with-outputs)
  ((definition :initarg :definition :initarg :type :reader definition
               :documentation "Block type definition")
   (components :initform (make-hash-table) :reader components
               :documentation "locally named components in this entity"))
  (:documentation "A logic block"))

(defmethod initialize-inputs ((b logic-block) &rest args)
  (getf (get-block-definition (definition b) args) :inputs))

(defmethod initialize-outputs ((b logic-block)  &rest args)
  (getf (get-block-definition (definition b) args) :outputs))

(defmethod initialize-instance :after ((b logic-block) &rest args)
  ;; all inputs and outputs should be connectors
  (map 'nil #'(lambda(n) (change-class n 'connector))
       (append (inputs b) (outputs b)))
  (let ((def (get-block-definition (definition b) args))
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
           (when n (error "Input ~A of component ~A in block ~A not connected"
                          n name  (name b))))
         (let ((n (find nil (outputs component) :key #'connections)))
           (when n (error "Output ~A of component ~A in block ~A not connected"
                          n name  (name b)))))
     components)
    (let ((unconnected (find nil (inputs b) :key #'connections)))
      (when unconnected (error "Block Input ~A unconnected" unconnected)))
    (let ((unconnected (find nil (outputs b) :key #'connection)))
      (when unconnected (error "Block Output ~A unconnected" unconnected)))))

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
