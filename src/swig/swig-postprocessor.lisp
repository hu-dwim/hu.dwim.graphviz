
(defparameter *generated-cffi-file*
  (merge-pathnames
   (make-pathname :directory '(:relative "src")
                  :name "swig-output"
                  :type "lisp")
   (asdf:component-pathname (asdf:find-system :cl-graphviz))))

(defparameter *processed-cffi-file*
  (merge-pathnames
   (make-pathname :name "swig-output-processed"
                  :type "lisp")
   *generated-cffi-file*))

(defparameter *replacements*
  '(("#o(1<<1)"   . "2")
    ("!(0)"       . "1")
    ("(t "        . "(t_ ")
    ("(arg1 )"    . "")
    ("(arg2 )"    . "")

    ("(1<<0)|(1<<1)|(1<<2)" . "7")
    ("(1<<0)|(1<<1)"        . "3")

    ("1<<0" . "1")
    ("1<<1" . "2")
    ("1<<2" . "4")
    ("1<<3" . "8")
    ("1<<4" . "16")
    ("1<<5" . "32")
    ("1<<6" . "64")
    ("1<<7" . "128")))

(flet ((replace-all (string part replacement &key (test #'char=))
         "Returns a new string in which all the occurences of the part 
is replaced with replacement."
         (with-output-to-string (out)
                                (loop with part-length = (length part)
                                      for old-pos = 0 then (+ pos part-length)
                                      for pos = (search part string
                                                        :start2 old-pos
                                                        :test test)
                                      do (write-string string out
                                                       :start old-pos
                                                       :end (or pos (length string)))
                                      when pos do (write-string replacement out)
                                      while pos))))
  (with-open-file (output *processed-cffi-file* :direction :output :if-exists :supersede)
    (write-line "(in-package :graphviz)" output)
    (with-open-file (input *generated-cffi-file*)
      (loop for line = (read-line input nil)
            while line
            do
            (loop for (match . replacement) in *replacements* do
                  (setf line (replace-all line match replacement)))
            (write-line line output)))))

