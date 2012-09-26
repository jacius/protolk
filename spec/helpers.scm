
(use test miscmacros)


;;; Wrap the test egg to be more RSpec-ish

;;; (describe "the thing being described"
;;;   (it "should do something"
;;;     (some expression that returns a truthy value on success)))

;;; Group related specs together
(define-syntax describe
  (syntax-rules ()
    ((describe summary . body)
     (test-group summary . body))))

;;; Tests that the body returns a truthy value when run
(define-syntax it
  (syntax-rules ()
    ((it summary . test-expressions)
     (test-assert summary (begin . test-expressions)))))


;;;;;;;;;;;;;;;;;;;;;;
;; TEST EXPRESSIONS
;;

;;; (raises? (exn-kind ...) body ...)
;;;
;;; Runs the body, watching for an exception matching all the
;;; specified kind(s) to be raised. If the body runs to completion,
;;; returns #f. If a matching exception is raised, returns the
;;; exception as a condition object.  If a non-matching exception is
;;; raised, the it will pass through. Use () to match any kind of
;;; exception. Examples:
;;;
;;;   (raises? ()
;;;     (vector-ref '#(0) 0))
;;;   ; #f
;;;
;;;   (raises? ()
;;;     (vector-ref '#(0) 1))
;;;   ; #<condition: (exn bounds)>
;;;
;;;   (raises? (bounds)
;;;     (vector-ref '#(0) 1))
;;;   ; #<condition: (exn bounds)>
;;;
;;;   (raises? (arithmetic)
;;;     (vector-ref '#(0) 1))
;;;   ; Error: (vector-ref) out of range ...
;;;
(define-syntax raises?
  (syntax-rules ()
    ((raises? (expected-exns ...) body ...)
     (condition-case
      (begin body ... #f)
      (var (expected-exns ...) var)))))
