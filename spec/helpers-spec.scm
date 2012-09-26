
;;; Speccing the spec helpers... SPECEPTION.

(load-relative "helpers")


(define some-global-var 1)
(define some-global-var2 2)
(define (some-global-fn) 'fn)
(define (some-global-fn2) (some-global-fn))

(describe "with-replacements"
  (it "sets the given variables within the body"
    (with-replacements ((some-global-var 5)
                        (some-global-var2 6))
      (equal? 11 (+ some-global-var some-global-var2))))

  (it "resets the variables after the body"
    (with-replacements ((some-global-var 5)
                        (some-global-var2 6))
      'noop)
    (equal? 3 (+ some-global-var some-global-var2)))

  (it "resets the variables even if the body has an error"
    (condition-case
     (with-replacements ((some-global-var 5)
                         (some-global-var2 6))
       (error "Boom"))
     (x () x))
    (equal? 3 (+ some-global-var some-global-var2)))

  (it "can be used to set just one variable"
    (with-replacements ((some-global-var 5))
      (equal? 7 (+ some-global-var some-global-var2))))

  (it "can be used to set local variables"
    (let ((a 1) (b 2))
      (with-replacements ((a 5) (b 6))
        (equal? 11 (+ a b)))))

  (it "can be used to set global function definitons"
    (with-replacements ((some-global-fn (lambda () 'new-fn)))
      (equal? 'new-fn (some-global-fn2)))))
