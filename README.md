# 3am

Just a tiny wrapper around [1am](https://github.com/lmj/1am).

## Synopsis

The entire API consists of: `test`, `is`, `signals`, `expands`, `run`, and
`*tests*`.

    (defpackage :example (:use :cl :3am))
    (in-package :example)
    
    (test foo-test
      (is (= 1 1))
      (is (zerop 0)))
    
    (test bar-test
      (signals simple-error
        (error "bar!")))
    
    add example for expands

Running:

    CL-USER> (in-package :example)
    #<PACKAGE "EXAMPLE">
    
    EXAMPLE> (run) ; run all tests
    BAR-TEST.
    FOO-TEST..
    Success: 2 tests, 3 checks.
    
    EXAMPLE> (foo-test) ; tests are just functions
    FOO-TEST..
    Success: 1 test, 2 checks.
    
    EXAMPLE> (run '(bar-test)) ; run particular tests
    BAR-TEST.
    Success: 1 test, 1 check.
    
    EXAMPLE> (setf *tests* '(foo-test)) ; set the default tests to run
    (FOO-TEST)
    EXAMPLE> (run)
    FOO-TEST..
    Success: 1 test, 2 checks.

## Overview

3am is just a tiny wrapper around 1am. The philosophy of 3m tries to stay
alinged with the one of 1am, i.e., simple is better; however, few additional
functions might occasionally be added to 3am to support for common testing
operations like: "what does this form expand to?", or "what's this form
output?".

## Workflow

Exactly like 1am, instead of a hierarchy of test suites 3am has a flat list of
tests in `*tests*`. When a test is compiled, it is added to `*tests*`.
A typical workflow might be:

1. Run all tests to verify they pass.

2. Set `*tests*` to `nil`, then compile specific tests that are
   relevant to some feature, perhaps writing new tests in the process.
   Now `*tests*` contains only those tests.

3. When changes for that feature are complete, recompile all tests to
   confirm those changes, which will add them back to `*tests*`.

4. Run all tests again to check that nothing is messed up.

## API

* [special variable] **`*tests*`** -- A list of tests; the default
  argument to `run`.

* [macro] **`test`** `name` *`&body`* `body` -- Define a test function
  and add it to `*tests*`.

* [macro] **`is`** `form` -- Assert that `form` evaluates to non-nil.

* [macro] **`signals`** `condition` *`&body`* `body` -- Assert that
  `body` signals a condition of type `condition`.

* [function] **`run`** *`&optional`* `tests` -- Run each test in the
  sequence `tests`. Default is `*tests*`.
