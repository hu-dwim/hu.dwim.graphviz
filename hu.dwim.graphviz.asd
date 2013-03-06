;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.graphviz
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>")
  :description "Graphviz layouting using CFFI bindings."
  :depends-on (:cffi :metabang-bind)
  :components ((:module "source"
                :components ((:file "package")
                             (:file "cffi" :depends-on ("package"))
                             (:file "graphviz" :depends-on ("cffi"))))))
