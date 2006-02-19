(in-package :common-lisp-user)

(defpackage "ASDF-CL-GRAPHVIZ" (:use #:cl #:asdf))
(in-package "ASDF-CL-GRAPHVIZ")

(defsystem cl-graphviz 
  :version "0.1"
  :author "Attila Lendvai <attila.lendvai@gmail.com>"
  :maintainer "Attila Lendvai <attila.lendvai@gmail.com>"
  :licence "MIT Style License"
  :description "CFFI interface for GraphViz"
  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "cl-graphviz" :depends-on ("package" "swig"))
             (:module "swig" :depends-on ("package")
                      :components
                      ((:file "swig-output-processed")
                       (:file "swig-output-overrides" :depends-on ("swig-output-processed"))
                       (:static-file "generate.sh")
                       (:static-file "swig.i"))))))
  :depends-on (:cffi :iterate))
