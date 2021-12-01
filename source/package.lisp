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

  (:export #:layout-dot-format
           #:edge-between
           #:node-name
           #:node-coordinate
           #:node-size
           #:graph-bounding-box

           #:iterate-edge-beziers
           #:bezier-points))

(in-package :hu.dwim.graphviz)

(define-foreign-library libgvc
  (:unix (:or "libgvc.so" "libgvc.so.4" "libgvc32.so.4"))
  (:darwin "libgvc.so")
  (:windows ("libgvc.dll" "msvcrt.dll"))
  (t "libgvc"))

;; TODO make this lazy...
(load-foreign-library 'libgvc)
