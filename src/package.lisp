(in-package common-lisp-user)

(defpackage :cl-graphviz
  (:use :cl :cffi :iterate :bind)
  (:nicknames :graphviz)
  (:documentation "CL-GraphViz is a CFFI interface for GraphViz.")
  
  (:export 
   #:layout-dot-format
   #:edge-between
   #:node-name
   #:node-coordinate
   #:node-size
   #:graph-bounding-box

   #:edge-iterate-beziers
   #:bezier-points

   #:gvContext
   #:agmemread
   #:gvLayout
   #:gvFreeLayout
   #:agclose
   #:gvFreeContext
   #:agwrite))