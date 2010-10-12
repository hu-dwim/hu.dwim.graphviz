;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(defpackage :hu.dwim.graphviz
  (:use :cffi
        :common-lisp
        :metabang-bind)

  (:export #:*graphviz-foreign-library-directories*

           #:layout-dot-format
           #:edge-between
           #:node-name
           #:node-coordinate
           #:node-size
           #:graph-bounding-box

           #:iterate-edge-beziers
           #:bezier-points))

(in-package :hu.dwim.graphviz)

(defvar *graphviz-foreign-library-directories* (list "/usr/lib/graphviz/" "/usr/lib/"))

(define-foreign-library libgvc
  (:unix (:or "libgvc.so" "libgvc.so.4" "libgvc32.so.4"))
  (:darwin "libgvc.so")
  (:windows ("libgvc.dll" "msvcrt.dll"))
  (t "libgvc"))

;; TODO make this lazy...
(let ((*foreign-library-directories* *graphviz-foreign-library-directories*))
  (load-foreign-library 'libgvc))
