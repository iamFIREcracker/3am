(defpackage #:3am
  (:use #:cl)
  (:import-from #:1am #:test #:is #:signals #:run #:*tests*)
  (:export #:test #:is #:signals #:run #:*tests* #:eval/test))
(in-package #:3am)

(defmacro eval/test (name &body body)
  "Define a test function, add it to `*tests*' and then run it immediately.

  Note: the test is immediately run only if the macro is directly evaluated in
  the REPL, i.e., if neither `*compile-file-pathname*' nor `*load-pathname*'
  are set."
  `(prog1 (test ,name ,@body)
     (unless (or *compile-file-pathname* *load-pathname*)
       (,name))))
