;; Core Package Definitions
;; Copyright (C) 2007 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;; All the public protocol interfaces should appear here.

;;; Code:

(in-package :cl-user)

(defpackage :logsim
  (:documentation "Some common declarations and interfaces for LENS")
  (:use :cl)
  (:import-from :alg
                #:make-binary-heap #:enqueue #:dequeue #:empty-p #:size)
  )
