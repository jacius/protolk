
(use test miscmacros)


;;; Wrap the test egg to be more RSpec-ish

;;; (describe "the thing being described"
;;;   (it "should do something"
;;;     (some expression that returns a truthy value on success)))

;;; Group related specs together
(define-syntax describe
  (er-macro-transformer
   (lambda (exp rename compare)
     `(,(rename 'test-group) ,@(cdr exp)))))

;;; Tests that the body returns a truthy value when run
(define-syntax it
  (er-macro-transformer
   (lambda (exp rename compare)
     (let ((summary (cadr exp))
           (test-expr (cddr exp)))
       `(,(rename 'test-assert) ,summary
         ,@test-expr)))))


;;;;;;;;;;;;;;;;;;;;;;
;; TEST EXPRESSIONS
;;

;;; (raises-exception? (exn-kind) body ...)
;;;
;;; Runs the body, watching for an exception of the specified kind(s)
;;; to be raised. If the body runs to completion, returns #f. If a
;;; matching exception is raised, returns the exception as a condition
;;; object.  If a non-matching exception is raised, the it will pass
;;; through. Use () to match any kind of exception.  Examples:
;;;
;;;   (raises-exception? ()
;;;     (vector-ref '#(0) 0))
;;;   ; #f
;;;
;;;   (raises-exception? ()
;;;     (vector-ref '#(0) 1))
;;;   ; #<condition: (exn bounds)>
;;;
;;;   (raises-exception? (bounds)
;;;     (vector-ref '#(0) 1))
;;;   ; #<condition: (exn bounds)>
;;;
;;;   (raises-exception? (arithmetic)
;;;     (vector-ref '#(0) 1))
;;;   ; Error: (vector-ref) out of range ...
;;;
(define-syntax raises-exception?
  (er-macro-transformer
   (lambda (expression rename compare)
     (let ((expected-exn (cadr expression))
           (body (cddr expression)))
       `(,(rename 'condition-case)
         (,(rename 'begin) ,@body #f)
         (var ,expected-exn var))))))
