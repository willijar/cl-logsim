;;; LENS System definition -*- Lisp -*-
;; Copyright (C) 2011 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :cl-user)

(asdf:defsystem :logsim
    :description "Lisp Educational Logic Simulator (logsim)"
    :author "Dr. John A.R. Williams <J.A.R.Williams@aston.ac.uk>"
    :maintainer "Dr. John A.R. Williams <J.A.R.Williams@aston.ac.uk>"
    :licence "GPL v3"
    :version "2.0"
    :long-description "A Simple Logic Simulator in LISP "
    :depends-on (:clrs)
    :components
    ((:file "defpackage")
     (:file "common" :depends-on ("defpackage"))
     (:file "compatibility" :depends-on ("defpackage"))
     (:file "simulator" :depends-on
            ("defpackage" "common" "compatibility"))
     (:file "entity" :depends-on ("simulator"))
     (:file "sources" :depends-on ("entity"))
     (:file "logic" :depends-on ("entity"))
     (:file "monitor" :depends-on ("entity"))
     (:file "latches" :depends-on ("entity"))
     (:file "block" :depends-on ("entity"))))
