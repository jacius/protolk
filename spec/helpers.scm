
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


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MOCKING AND STUBBING
;;

;;; Set the value(s) of the given variable(s) for the duration of the
;;; body, then set them back to their original values. The variables
;;; can be global or local variables. This can be used to temporarily
;;; redefine global functions, among other sneaky tricks.
;;;
;;; Example:
;;;
;;;   (define a 3)
;;;   (define b 8)
;;;   (define (f) (+ a b))
;;;   (f) ; => 11
;;;   (with-replacements ((a 4) (b -11))
;;;     (f)) ; => -7
;;;
(define-syntax with-replacements
  (syntax-rules ()
    ((with-replacements ((some-def new-val) ...) . body)
     (let ((old-defs (list (cons 'some-def some-def) ...)))
       (dynamic-wind
         (lambda ()
           (set! some-def new-val) ...)
         (lambda () . body)
         (lambda ()
           (set! some-def (cdr (assoc 'some-def old-defs)))
           ...))))))


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
