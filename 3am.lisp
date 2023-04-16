(defpackage #:3am
  (:use #:cl)
  (:import-from #:1am #:test #:is #:signals #:run #:*tests*)
  (:export #:test #:is #:signals #:run #:*tests*))
(in-package #:3am)
