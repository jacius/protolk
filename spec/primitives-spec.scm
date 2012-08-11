
(load-relative "helpers")

(load-relative "../primitives")
(import protolk-primitives)


(describe "pob pritimive record type"

  (describe "%make-pob"
    (it "succeeds when given a props alist and a methods alist"
      (not (raises-error? (%make-pob '() '()))))
    (it "fails when given no args"
      (raises-error? (%make-pob)))
    (it "fails when given only one arg"
      (raises-error? (%make-pob '())))
    (it "fails when given too many args"
      (raises-error? (%make-pob '() '() '()))))

  (describe "pob?"
    (it "returns #t when given a pob"
      (pob? (%make-pob '() '())))
    (it "returns #f when given a non-pob"
      (not (pob? 'foo)))
    (it "fails when given no args"
      (raises-error? (pob?))))

  
  (describe "%pob-props"
    (it "returns the pob's props alist"
      (equal? (%pob-props (%make-pob '((a . 1)) '()))
              '((a . 1))))
    (it "fails when given a non-pob"
      (raises-error? (%pob-props 'foo)))
    (it "fails when given no args"
      (raises-error? (%pob-props))))

  (describe "%pob-set-props!"
    (it "modifies the pob's props alist"
      (let ((pob (%make-pob '() '())))
        (%pob-set-props! pob '((a . 1)))
        (equal? (%pob-props pob)
                '((a . 1)))))
    (it "fails when given a non-pob"
      (raises-error? (%pob-set-props! 'foo '((a . 1)))))
    (it "fails when given no args"
      (raises-error? (%pob-set-props!)))
    (it "fails when given only one arg"
      (raises-error? (%pob-set-props! (%make-pob '() '())))))

  
  (describe "%pob-methods"
    (it "returns the pob's methods alist"
      (let ((meth (lambda (pob) #t)))
        (equal? (%pob-methods (%make-pob '() `((m . ,meth))))
                `((m . ,meth)))))
    (it "fails when given a non-pob"
      (raises-error? (%pob-methods 'foo)))
    (it "fails when given no args"
      (raises-error? (%pob-methods))))

  (describe "%pob-set-methods!"
    (let ((fn (lambda (pob) #t)))
      (it "modifies the pob's methods alist"
        (let ((pob (%make-pob '() '())))
          (%pob-set-methods! pob `((m . ,fn)))
          (equal? (%pob-methods pob)
                  `((m . ,fn)))))
      (it "fails when given a non-pob"
        (raises-error? (%pob-set-methods! 'foo `((m . ,fn)))))
      (it "fails when given no args"
        (raises-error? (%pob-set-methods!)))
      (it "fails when given only one arg"
        (raises-error? (%pob-set-methods! (%make-pob '() '())))))))



(describe "primitive prop accessors"
  (describe "%has-prop?"
    (it "returns #t when the pob has a matching prop"
      (%has-prop? (%make-pob '((a . 1)) '()) 'a))
    (it "returns #f when the pob does not have a matching prop"
      (not (%has-prop? (%make-pob '((a . 1)) '()) 'foo)))
    (it "fails when given a non-pob"
      (raises-error? (%has-prop? 'foo 'a)))
    (it "fails when no prop name is specified"
      (raises-error? (%has-prop? (%make-pob '((a . 1)) '())))))

  (describe "%prop"
    (it "returns the value of a matching prop"
      (equal? (%prop (%make-pob '((a . 1)) '()) 'a)
              1))
    (it "returns #<unspecified> when there is no matching prop"
      (equal? (%prop (%make-pob '((a . 1)) '()) 'foo)
              (void)))
    (it "optionally accepts a default return value"
      (equal? (%prop (%make-pob '((a . 1)) '()) 'foo 'result)
              'result))
    (it "fails when given a non-pob"
      (raises-error? (%prop 'foo 'bar)))
    (it "fails when no prop name is specified"
      (raises-error? (%prop (%make-pob '((a . 1)) '())))))

  (describe "%set-prop!"
    (it "sets the specified prop in the pob"
      (let ((pob (%make-pob '() '())))
        (%set-prop! pob 'a 1)
        (equal? (%prop pob 'a) 1)))
    (it "replaces the value of existing props with that name"
      (let ((pob (%make-pob '((a . 1)) '())))
        (%set-prop! pob 'a 2)
        (equal? (%prop pob 'a) 2)))
    (it "fails when given no args"
      (raises-error? (%set-prop!)))    
    (it "fails when given a non-pob"
      (raises-error? (%set-prop! 'foo 'a 1)))
    (it "fails when no prop name or value is specified"
      (raises-error? (%set-prop! (%make-pob '() '()))))
    (it "fails when no value is specified"
      (raises-error? (%set-prop (%make-pob '() '()) 'a))))

  (describe "%unset-prop!"
    (it "removes all matching props from the pob"
      (let ((pob (%make-pob '((a . 2) (a . 1)) '())))
        (%unset-prop! pob 'a)
        (not (%has-prop? pob 'a))))
    (it "has no effect if there are no matching props"
      (let ((pob (%make-pob '() '())))
        (%unset-prop! pob 'a)
        (not (%has-prop? pob 'a))))
    (it "fails when given no args"
      (raises-error? (%unset-prop!)))    
    (it "fails when given a non-pob"
      (raises-error? (%unset-prop! 'foo 'a)))
    (it "fails when no prop name is specified"
      (raises-error? (%unset-prop! (%make-pob '() '()))))))


(test-exit)
