;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.graphviz)

(eval-when (:compile-toplevel :execute)

  ;;; enable a simple reader macro to read $foo in the :hu.dwim.graphviz.cffi package

  (defun symbol-reader (stream char)
    (declare (ignore char))
    (let ((body (let ((*package* #.(find-package :hu.dwim.graphviz.cffi)))
                  (read stream t nil t))))
      `(quote ,body)))

  (defun enable-symbol-reader ()
    (setf *readtable* (copy-readtable *readtable*))
    (set-macro-character #\$ 'symbol-reader))

  (enable-symbol-reader)

  #+#.(cl:when (cl:find-package "SWANK") '(:and))
  (unless (assoc :hu.dwim.graphviz swank:*readtable-alist* :test #'string=)
    (flet ((doit (&rest packages)
             (dolist (package packages)
               (push (cons package *readtable*) swank:*readtable-alist*))))
      (doit :hu.dwim.graphviz))))

;;;;;;
;;; public stuff to use comes here

(defmacro with-gv-context (var-name &body forms)
  `(let ((,var-name (hu.dwim.graphviz.cffi:gv-context)))
    (unwind-protect
         (progn
           ,@forms)
      (hu.dwim.graphviz.cffi:gv-free-context ,var-name))))

(defun to-point (point)
  ;; TODO finish dealing with the rest of the warnings...
  (list (foreign-slot-value point '(:struct hu.dwim.graphviz.cffi:point) $x)
        (foreign-slot-value point '(:struct hu.dwim.graphviz.cffi:point) $y)))

(defun node-info (node)
  (foreign-slot-pointer node $node-t $u))

(defun node-name (node)
  (foreign-string-to-lisp (foreign-slot-value node $node-t $name)))

(defun node-coordinate (node)
  (to-point (foreign-slot-value (node-info node) $agnodeinfo-t $coord)))

(defun node-size (node)
  (list (foreign-slot-value (node-info node) $agnodeinfo-t $width)
        (foreign-slot-value (node-info node) $agnodeinfo-t $height)))

(defun edge-between (edge)
  (list (foreign-slot-value edge $edge-t $tail)
        (foreign-slot-value edge $edge-t $head)))

(defun spline-count (splines)
  (foreign-slot-value splines $splines $size))

(defun splines-of-edge (edge)
  (let ((edge-info (foreign-slot-pointer edge $agedge-t $u)))
    (foreign-slot-value edge-info $agedgeinfo-t $spl)))

(defun splines-bezier-at (splines index)
  (let ((base (foreign-slot-value splines $splines $list)))
    (inc-pointer base (* index (foreign-type-size $bezier)))))

; TODO what is this actually?
(defun bezier-start-point (bezier)
  (to-point (foreign-slot-value bezier $bezier $sp)))

; TODO what is this actually?
(defun bezier-end-point (bezier)
  (to-point (foreign-slot-value bezier $bezier $ep)))

(defun bezier-point-count (bezier)
  (foreign-slot-value bezier $bezier $size))

(defun bezier-point-at (bezier index)
  (let* ((points (foreign-slot-value bezier $bezier $list))
         (point (mem-aref points $point index)))
    (to-point point)))

;(defun bezier-points (bezier)
;  (bind (((startx starty) (bezier-start-point bezier))
;         ((endx endy) (bezier-end-point bezier))
;         (result '()))
;    (push (list startx starty) result)
;    (bezier-iterate-points bezier
;                           (lambda (x y)
;                             (push (list x y) result)))
;    (push (list endx endy) result)
;    (nreverse result)))

(defun bezier-points (bezier)
  (let ((result '()))
    (iterate-bezier-points bezier
                           (lambda (x y)
                             (push (list x y) result)))
    (nreverse result)))

(defun iterate-bezier-points (bezier visitor)
  (loop for i :from 0 :below (bezier-point-count bezier)
        do (bind (((x y) (bezier-point-at bezier i)))
             (funcall visitor x y))))

(defun iterate-edge-beziers (edge visitor)
  (let ((splines (splines-of-edge edge)))
    (loop for i :from 0 :below (spline-count splines)
          do (let ((bezier (splines-bezier-at splines i)))
               (funcall visitor bezier)))))

(defun edge-type (edge)
  (let ((edge-info (foreign-slot-pointer edge $agedge-t $u)))
    (foreign-slot-value edge-info $agedgeinfo-t $edge-type)))

(defun edge-label (edge)
  (let ((edge-info (foreign-slot-pointer edge $agedge-t $u)))
    (foreign-slot-pointer edge-info $agedgeinfo-t $label)))

(defun head-label (edge)
  (let ((edge-info (foreign-slot-pointer edge $agedge-t $u)))
    (foreign-slot-pointer edge-info $agedgeinfo-t $head-label)))

(defun tail-label (edge)
  (let ((edge-info (foreign-slot-pointer edge $agedge-t $u)))
    (foreign-slot-pointer edge-info $agedgeinfo-t $tail-label)))

(defun label-coordinate (label)
  (to-point (foreign-slot-value label $textlabel-t $p)))

(defun box-lower-left (box)
  (to-point (foreign-slot-value box $box $ll)))

(defun box-upper-right (box)
  (to-point (foreign-slot-value box $box $ur)))

(defun graph-bounding-box (graph)
  (let* ((graph-info (foreign-slot-pointer graph $agraph-t $u))
         (bounding-box (foreign-slot-pointer graph-info $agraphinfo-t $bb)))
    (list (box-lower-left bounding-box) (box-upper-right bounding-box))))

(defgeneric layout-dot-format (graph-description &key
                                                 algorithm
                                                 node-visitor
                                                 edge-visitor
                                                 graph-visitor))

(defmethod layout-dot-format ((graph-description string)
                              &key
                              (algorithm "dot")
                              node-visitor
                              edge-visitor
                              graph-visitor)
  (unless (or node-visitor edge-visitor)
    (error "At least one visitor is needed"))
  (with-gv-context context
    (let ((graph nil)
          (layout-result-code nil))
      (unwind-protect
           (progn
             (setf graph (with-foreign-string (str graph-description)
                           (hu.dwim.graphviz.cffi:agmemread str)))
             (when (null-pointer-p graph)
               (error "Error from agmemread(), probably invalid graph description"))
             (setf layout-result-code (with-foreign-string (algorithm algorithm)
                                        (hu.dwim.graphviz.cffi:gv-layout context graph algorithm)))
             (when (not (eql layout-result-code 0))
               (error "gvLayout returned with ~A" layout-result-code))
             (when graph-visitor
               (funcall graph-visitor graph))

             (loop for node = (hu.dwim.graphviz.cffi:agfstnode graph)
                            :then (hu.dwim.graphviz.cffi:agnxtnode graph node)
                   until (null-pointer-p node)
                   do (progn
                        (when node-visitor
                          (funcall node-visitor node))

                        (when edge-visitor
                          (loop for edge = (hu.dwim.graphviz.cffi:agfstedge graph node)
                                         :then (hu.dwim.graphviz.cffi:agnxtedge graph edge node)
                                until (null-pointer-p edge)
                                do (funcall edge-visitor edge))))))
        (when layout-result-code
          (hu.dwim.graphviz.cffi:gv-free-layout context graph))
        (when graph
          (hu.dwim.graphviz.cffi:agclose graph))))))
