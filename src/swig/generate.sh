#!/bin/sh

GRAPHVIZ_INCLUDE_DIR=/usr/include/graphviz

swig -cffi -I$GRAPHVIZ_INCLUDE_DIR swig.i

sbcl --load swig-postprocessor.lisp --eval "(quit)"

