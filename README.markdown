# Butter - Unit testing framework for Common Lisp

Butter is an unit testing framework for Common Lisp. This is similar to clojure.test in Clojure.

## Usage

    (defpackage :your-package
      (:use :cl
            :butter))
    (in-package :your-package)

    (deftest first-test-case
      (is (= 10 (+ 1 2 3 4)))
      (is (equal '(garlic butter) (append '(garlic) '(butter)))))

    (deftest second-test-case
      (is (oddp 11) "11 is an odd number.")
      (is (evenp 12) "12 is an even number."))

    (run-tests)

## Assertions

The "is" macro makes an assertion of an expression:

    (is (= 10 (+ 1 2 3 4)))
    (is (equal '(a b c) (cons 'a '(b c))))
    (is (keywordp :x))

You can use this macro directly at the REPL:

    > (is (= 10 (+ 1 2 3 4)))
    T

This macro prints a message like the following if an assertion fails:

    > (is (= 8 (+ 2 4)))
    FAIL in TOP LEVEL
    expected: (= 8 (+ 2 4))
      actual: (NOT (= 8 6))
    NIL

You can confirm the original expression at the "expected" line, and what actually happened at the "actual" line.

An "is" expression returns T if its assertion passes, otherwise NIL.

### Special assertions

There are two special assertions for testing expressions.

#### :signal

The ":signal" special assertion tests if a condition of a class is signalled:

    (is (:signal type-error (length 'a)))

The ":signal" assertion passes immediately when an expected condition was signalled.

#### :print

The ":print" special assertion tests printing from forms to a specified stream:

    (is (:print *standard-output* "garlic butter!" (format t "~A butter!" "garlic")))

You can add special assertions with the "define-special-assertion" macro in the "butter.extending" package.

### "are" macro

The "are" macro makes multiple assertions with an expansion form:

    (are (expected x y) `(= ,expected (+ ,x ,y))
      (10 3 7)
      (15 8 7)
      (9 6 3))

This form is expanded to:

    (progn
      (is (= 10 (+ 3 7)))
      (is (= 15 (+ 8 7)))
      (is (= 9 (+ 6 3))))

## Defining tests

The "deftest" macro defines a test in the current package:

    (deftest appending-lists
      (is (equal '(1 2 3 4) (append '(1 2) '(3 4))))
      (is (equal '(a b c) (append '(a b c) ())))
      (is (equal '(garlic butter) (append () '(garlic butter)))))
    
    (deftest concatenating-strings
      (is (string= "ABCDE" (concatenate 'string "ABC" "DE")))
      (is (string= "GARLIC BUTTER" (concatenate 'string "GARLIC" " " "BUTTER"))))

## Running tests

The "run-tests" function runs tests in specified packages:

    (run-tests :your-package :other-package)

This function runs tests in the current package if you don't specify any packages.

The "run-all-tests" function runs all tests in all packages:

    (run-all-tests)

## Reporters

Test reporter shows you results fo tests. The default test reporter prints results to \*standard-output\*. You can use an another test reporter with the "with-reporter" macro:

    (with-reporter (make-instance 'butter.cui:cui-reporter :verbose t)
      (run-tests))

The "cui-reporter" class in the "butter.cui" package prints results to a stream. You can change the behavior of the reporter by following initargs:

  - stream - An output-stream. The reporter prints results to the stream. It's \*standard-output\* by default.
  - verbose - If the value is not NIL, the reporter reports passed assertions, otherwise the reporter reports just only failed assertions. It's NIL by default.
  - invoke-debugger - If the value is not NIL, the debugger is invoked when an error has occurred, otherwise the assertion is exited as failed. It's NIL by default.

## Extending Butter

### Adding special assertions

The "define-special-assertion" adds a special assertion like defmacro:

    (define-special-assertion my-eql (expected form)
      (let ((value-symbol (gensym)))
        `(let ((,value-symbol ,form))
           (values (eql ,value-symbol ,expected) ,value-symbol))))

This form makes the "my-eql" new special assertion. You can use it with the "is" macro:

    (is (my-eql 10 (+ 3 7)))

Forms made from special-assertions must return two values: The first is a non NIL value if the assertion passes, otherwise NIL, and the second is a some value that represents what actually happened.
