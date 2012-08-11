
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

;;; Returns #t if the body raises an error when run, otherwise #f.
(define-syntax raises-error?
  (er-macro-transformer
   (lambda (exp rename compare)
     `(,(rename 'handle-exceptions) exn
       #t
       (,(rename 'begin)
        ,@(cdr exp)
        #f)))))
