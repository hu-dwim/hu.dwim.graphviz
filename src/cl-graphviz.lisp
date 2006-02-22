(in-package :graphviz)

(defvar *graphviz-library-path* "/usr/lib/graphviz/")

(define-foreign-library libgvc
  (:unix (:or "libgvc.so" "libgvc32.so"))
  (:darwin "libgvc.so")
  (:windows "libgvc.dll" "msvcrt.dll"))

(let ((*foreign-library-directories* (list *graphviz-library-path*)))
  (load-foreign-library 'libgvc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; public stuff to use comes here
;;;

(defmacro with-gvContext (var-name &body forms)
  `(let ((,var-name (gvContext)))
    (unwind-protect
         (progn
           ,@forms)
      (gvFreeContext ,var-name))))

(defun node-info (node)
  (foreign-slot-pointer node 'agnode_t 'u))

(defun node-name (node)
  (foreign-slot-value node 'agnode_t 'name))

(defun node-coordinate (node)
  (list (foreign-slot-value (node-info node) 'agnodeinfo_t 'xcoord)
        (foreign-slot-value (node-info node) 'agnodeinfo_t 'ycoord)))

(defun node-size (node)
  (list (foreign-slot-value (node-info node) 'agnodeinfo_t 'width)
        (foreign-slot-value (node-info node) 'agnodeinfo_t 'height)))

(defun edge-between (edge)
  (list (foreign-slot-value edge 'agedge_t 'tail)
        (foreign-slot-value edge 'agedge_t 'head)))

(defun spline-count (splines)
  (foreign-slot-value splines 'splines 'size))

(defun splines-of-edge (edge)
  (let ((edge-info (foreign-slot-pointer edge 'agedge_t 'u)))
    (foreign-slot-value edge-info 'agedgeinfo_t 'spl)))

(defun splines-bezier-at (splines index)
  (let ((base (foreign-slot-value splines 'splines 'list)))
    (inc-pointer base (* index (foreign-type-size 'bezier)))))

; TODO what is this actually?
(defun bezier-start-point (bezier)
  (list (foreign-slot-value bezier 'bezier 'spx)
        (foreign-slot-value bezier 'bezier 'spy)))

; TODO what is this actually?
(defun bezier-end-point (bezier)
  (list (foreign-slot-value bezier 'bezier 'epx)
        (foreign-slot-value bezier 'bezier 'epy)))

(defun bezier-point-count (bezier)
  (foreign-slot-value bezier 'bezier 'size))

(defun bezier-point-at (bezier index)
  (let ((base (foreign-slot-value bezier 'bezier 'list)))
    (setf base (inc-pointer base (* index (foreign-type-size 'point))))
    (list (foreign-slot-value base 'point 'x)
          (foreign-slot-value base 'point 'y))))

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
    (bezier-iterate-points bezier
                           (lambda (x y)
                             (push (list x y) result)))
    (nreverse result)))

(defun bezier-iterate-points (bezier visitor)
  (iter (for i from 0 below (bezier-point-count bezier))
        (bind (((x y) (bezier-point-at bezier i)))
          (funcall visitor x y))))

(defun edge-iterate-beziers (edge visitor)
  (let ((splines (splines-of-edge edge)))
    (iter (for i from 0 below (spline-count splines))
          (let ((bezier (splines-bezier-at splines i)))
            (funcall visitor bezier)))))   

(defun edge-type (edge)
  (let ((edge-info (foreign-slot-pointer edge 'agedge_t 'u)))
    (foreign-slot-value edge-info 'agedgeinfo_t 'edge_type)))

;;; Here is the racionale for this (tough i have no clue what it is) :
;;; #define ED_edge_type(e) (e)->u.edge_type
;;; #define ED_to_orig(e) (e)->u.to_orig
;;; while (ED_edge_type(e) != NORMAL)
;;;	e = ED_to_orig(e);
(defun edge-normalize (edge)
  (do* ((edge-info nil (foreign-slot-pointer current 'agedge_t 'u))
        (current edge (foreign-slot-value edge-info 'agedgeinfo_t 'to_orig)))
       ((eql (edge-type edge) 0) edge)))

(defun point (point)
  (list (foreign-slot-value point 'point 'x)
        (foreign-slot-value point 'point 'y)))

(defun box-lower-left (box)
  (point box))

(defun box-upper-right (box)
  (point (inc-pointer box (foreign-type-size 'point))))

(defun graph-bounding-box (graph)
  (let* ((graph-info (foreign-slot-pointer graph 'agraph_t 'u))
         (bounding-box (foreign-slot-pointer graph-info 'agraphinfo_t 'bb)))
    (list (box-lower-left bounding-box) (box-upper-right bounding-box))))

(defmethod layout-dot-format ((graph-description string)
                              &key
                              (algorithm "dot")
                              node-visitor
                              edge-visitor
                              graph-visitor)
  (unless (or node-visitor edge-visitor)
    (error "At least one visitor is needed"))
  (with-gvContext context
    (let ((graph nil)
          (layout-result-code nil))
      (unwind-protect
           (progn 
             (setf graph (agmemread graph-description))
             (when (null-pointer-p graph)
               (error "Error from agmemread(), probably invalid graph description"))
             (setf layout-result-code (gvLayout context graph algorithm))
             (when (not (eql layout-result-code 0))
               (error "gvLayout returned with ~A" layout-result-code))
             
             (when graph-visitor
               (funcall graph-visitor graph))

             (iter (for node first (agfstnode graph) then (agnxtnode graph node))
                   (until (null-pointer-p node))
                   (when node-visitor
                     (funcall node-visitor node))

                   (when edge-visitor
                     (iter (for edge
                                first (agfstedge graph node)
                                then (agnxtedge graph edge node))
                           (until (null-pointer-p edge))
                           (funcall edge-visitor (edge-normalize edge))))))
      (when layout-result-code (gvFreeLayout context graph))
      (when graph (agclose graph))))))



