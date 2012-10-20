
(load-relative "spec-helpers")
(load-relative "../protolk")
(import protolk
        protolk-primitives
        protolk-internal)
(import-for-syntax protolk)

(use extras)


;;;;;;;;;;;;;;;;;;;;
;; PROP ACCESSORS
;;

(describe "prop-reader"
  (it "accepts a prop name and returns a procedure"
    (procedure? (prop-reader 'some-prop)))

  (it "fails if given no args"
    (raises? (arity)
      (prop-reader)))

  (it "fails if given too many args"
    (raises? (arity)
      (prop-reader 'some-prop 'foo)))
  
  (describe "the procedure"
    (define some-prop-reader (prop-reader 'some-prop))
    (define pob1 (make-pob props: '((some-prop some-value))))

    (it "returns the given pob's value for the matching prop"
      (equal? (some-prop-reader pob1)
              'some-value))

    (it "fails if given a non-pob"
      (raises? (type)
        (some-prop-reader 'foo)))
    
    (it "fails if given no args"
      (raises? (arity)
        (some-prop-reader)))

    (it "fails if given too many args"
      (raises? (arity)
        (some-prop-reader pob1 'foo)))))


(describe "prop-writer"
  (it "accepts a prop name and returns a procedure"
    (procedure? (prop-writer 'some-prop)))

  (it "fails if given no args"
    (raises? (arity)
      (prop-writer)))

  (it "fails if given too many args"
    (raises? (arity)
      (prop-writer 'some-prop 'foo)))
  
  (describe "the procedure"
    (define some-prop-writer (prop-writer 'some-prop))

    (it "sets the matching prop's value in the given pob"
      (let ((pob (make-pob)))
        (some-prop-writer pob 'some-value)
        (equal? (%prop pob 'some-prop)
                'some-value)))

    (it "returns void"
      (let ((pob (make-pob)))
        (equal? (some-prop-writer pob 'some-value)
                (void))))
    
    (it "fails if given a non-pob"
      (raises? (type)
        (some-prop-writer 'foo 'bar)))
    
    (it "fails if given no args"
      (raises? (arity)
        (some-prop-writer)))

    (it "fails if given too many args"
      (let ((pob (make-pob)))
        (raises? (arity)
          (some-prop-writer pob 'some-value 'foo))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cond-expand
 ((not protolk-all-tests)
  (test-exit))
 (else))
