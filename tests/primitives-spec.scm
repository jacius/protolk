
(cond-expand
 ((not protolk-all-tests)
  (load-relative "helpers")
  (load-relative "../protolk-primitives")
  (import protolk
          protolk-primitives
          protolk-internal))
 (else))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; POB PRITIMIVE RECORD TYPE
;;

(describe "pob pritimive record type"

  (describe "%make-pob"
    (it "succeeds when given a base, a props alist, and a methods alist"
      (not (raises? ()
             (%make-pob #f '() '()))))
    (it "fails when given no args"
      (raises? ()
        (%make-pob)))
    (it "fails when given only one arg"
      (raises? ()
        (%make-pob #f)))
    (it "fails when given only two args"
      (raises? ()
        (%make-pob #f '())))
    (it "fails when given four or more many args"
      (raises? ()
        (%make-pob #f '() '() '()))))


  (describe "pob?"
    (it "returns #t when given a pob"
      (pob? (%make-pob #f '() '())))
    (it "returns #f when given a non-pob"
      (not (pob? 'notapob)))
    (it "fails when given no args"
      (raises? ()
        (pob?))))


  (describe "%pob-base"
    (it "returns the pob's base"
      (let ((base-pob (%make-pob #f '() '())))
        (equal? (%pob-base (%make-pob base-pob '() '()))
                base-pob)))
    (it "fails when given a non-pob"
      (raises? ()
        (%pob-base 'notapob)))
    (it "fails when given no args"
      (raises? ()
        (%pob-base))))

  (describe "%pob-set-base!"
    (it "modifies the pob's base alist"
      (let ((pob1 (%make-pob #f '() '()))
            (pob2 (%make-pob #f '() '())))
        (%pob-set-base! pob1 pob2)
        (equal? (%pob-base pob1) pob2)))
    (it "fails when given a non-pob"
      (raises? ()
        (%pob-set-base! 'notapob '((a . 1)))))
    (it "fails when given no args"
      (raises? ()
        (%pob-set-base!)))
    (it "fails when given only one arg"
      (raises? ()
        (%pob-set-base! (%make-pob '() '())))))


  (describe "%pob-props"
    (it "returns the pob's props alist"
      (equal? (%pob-props (%make-pob #f '((a . 1)) '()))
              '((a . 1))))
    (it "fails when given a non-pob"
      (raises? ()
        (%pob-props 'notapob)))
    (it "fails when given no args"
      (raises? ()
        (%pob-props))))

  (describe "%pob-set-props!"
    (it "modifies the pob's props alist"
      (let ((pob (%make-pob #f '() '())))
        (%pob-set-props! pob '((a . 1)))
        (equal? (%pob-props pob)
                '((a . 1)))))
    (it "fails when given a non-pob"
      (raises? ()
        (%pob-set-props! 'notapob '((a . 1)))))
    (it "fails when given no args"
      (raises? ()
        (%pob-set-props!)))
    (it "fails when given only one arg"
      (raises? ()
        (%pob-set-props! (%make-pob #f '() '())))))


  (define (fn pob) #t)

  (describe "%pob-methods"
    (it "returns the pob's methods alist"
      (equal? (%pob-methods (%make-pob #f '() `((m . ,fn))))
              `((m . ,fn))))
    (it "fails when given a non-pob"
      (raises? ()
        (%pob-methods 'notapob)))
    (it "fails when given no args"
      (raises? ()
        (%pob-methods))))

  (describe "%pob-set-methods!"
    (it "modifies the pob's methods alist"
      (let ((pob (%make-pob #f '() '())))
        (%pob-set-methods! pob `((m . ,fn)))
        (equal? (%pob-methods pob)
                `((m . ,fn)))))
    (it "fails when given a non-pob"
      (raises? ()
        (%pob-set-methods! 'notapob `((m . ,fn)))))
    (it "fails when given no args"
      (raises? ()
        (%pob-set-methods!)))
    (it "fails when given only one arg"
      (raises? ()
        (%pob-set-methods! (%make-pob #f '() '()))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRIMITIVE PROP ACCESSORS
;;

(describe "primitive prop accessors"

  (describe "%has-prop?"
    (it "returns #t when the pob has a matching prop"
      (%has-prop? (%make-pob #f '((a . 1)) '()) 'a))
    (it "returns #f when the pob does not have a matching prop"
      (not (%has-prop? (%make-pob #f '((a . 1)) '()) 'foo)))
    (it "fails when given a non-pob"
      (raises? ()
        (%has-prop? 'foo 'a)))
    (it "fails when no prop name is specified"
      (raises? ()
        (%has-prop? (%make-pob #f '((a . 1)) '())))))

  (describe "%prop"
    (it "returns the value of a matching prop"
      (equal? (%prop (%make-pob #f '((a . 1)) '()) 'a)
              1))
    (it "returns #<unspecified> when there is no matching prop"
      (equal? (%prop (%make-pob #f '((a . 1)) '()) 'foo)
              (void)))
    (it "optionally accepts a default return value"
      (equal? (%prop (%make-pob #f '((a . 1)) '()) 'foo 'result)
              'result))
    (it "fails when given a non-pob"
      (raises? ()
        (%prop 'foo 'bar)))
    (it "fails when no prop name is specified"
      (raises? ()
        (%prop (%make-pob #f '((a . 1)) '())))))

  (describe "%set-prop!"
    (it "sets the specified prop in the pob"
      (let ((pob (%make-pob #f '() '())))
        (%set-prop! pob 'a 1)
        (equal? (%prop pob 'a) 1)))
    (it "replaces the value of existing props with that name"
      (let ((pob (%make-pob #f '((a . 1)) '())))
        (%set-prop! pob 'a 2)
        (equal? (%prop pob 'a) 2)))
    (it "fails when given no args"
      (raises? ()
        (%set-prop!)))
    (it "fails when given a non-pob"
      (raises? ()
        (%set-prop! 'foo 'a 1)))
    (it "fails when no prop name or value is specified"
      (raises? ()
        (%set-prop! (%make-pob #f '() '()))))
    (it "fails when no value is specified"
      (raises? ()
        (%set-prop (%make-pob #f '() '()) 'a))))

  (describe "%unset-prop!"
    (it "removes all matching props from the pob"
      (let ((pob (%make-pob #f '((a . 2) (a . 1)) '())))
        (%unset-prop! pob 'a)
        (not (%has-prop? pob 'a))))
    (it "has no effect if there are no matching props"
      (let ((pob (%make-pob #f '() '())))
        (%unset-prop! pob 'a)
        (not (%has-prop? pob 'a))))
    (it "fails when given no args"
      (raises? ()
        (%unset-prop!)))
    (it "fails when given a non-pob"
      (raises? ()
        (%unset-prop! 'foo 'a)))
    (it "fails when no prop name is specified"
      (raises? ()
        (%unset-prop! (%make-pob #f '() '()))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRIMITIVE METHOD ACCESSORS
;;

(describe "primitive method accessors"
  (define (fn pob) #t)
  (define (fn2 pob) #f)

  (describe "%has-method?"
    (it "returns #t when the pob has a matching method"
      (%has-method? (%make-pob #f '() `((m . ,fn))) 'm))
    (it "returns #f when the pob does not have a matching method"
      (not (%has-method? (%make-pob #f '() `((m . ,fn))) 'foo)))
    (it "fails when given a non-pob"
      (raises? ()
        (%has-method? 'foo 'm)))
    (it "fails when no method name is specified"
      (raises? ()
        (%has-method? (%make-pob #f '() `((m . ,fn)))))))

  (describe "%method"
    (it "returns the value of a matching method"
      (equal? (%method (%make-pob #f '() `((m . ,fn))) 'm)
              fn))
    (it "returns #<unspecified> when there is no matching method"
      (equal? (%method (%make-pob #f '() `((m . ,fn))) 'foo)
              (void)))
    (it "optionally accepts a default return value"
      (equal? (%method (%make-pob #f '() `((m . ,fn))) 'foo fn)
              fn))
    (it "fails when given a non-pob"
      (raises? ()
        (%method 'foo 'm)))
    (it "fails when no method name is specified"
      (raises? ()
        (%method (%make-pob #f '() `((m . ,fn)))))))

  (describe "%set-method!"
    (it "sets the specified method in the pob"
      (let ((pob (%make-pob #f '() '())))
        (%set-method! pob 'm fn)
        (equal? (%method pob 'm) fn)))
    (it "replaces the value of existing methods with that name"
      (let ((pob (%make-pob #f '() `((m . ,fn)))))
        (%set-method! pob 'm fn)
        (equal? (%method pob 'm) fn)))
    (it "fails when given no args"
      (raises? ()
        (%set-method!)))
    (it "fails when given a non-pob"
      (raises? ()
        (%set-method! 'foo 'm fn)))
    (it "fails when no method name or value is specified"
      (raises? ()
        (%set-method! (%make-pob #f '() '()))))
    (it "fails when no value is specified"
      (raises? ()
        (%set-method (%make-pob #f '() '()) 'fn))))

  (describe "%unset-method!"
    (it "removes all matching methods from the pob"
      (let ((pob (%make-pob #f '() `((m . fn2) (m . fn)))))
        (%unset-method! pob 'm)
        (not (%has-method? pob 'm))))
    (it "has no effect if there are no matching methods"
      (let ((pob (%make-pob #f '() '())))
        (not (%has-method? pob 'm))))
    (it "fails when given no args"
      (raises? ()
        (%unset-method!)))
    (it "fails when given a non-pob"
      (raises? ()
        (%unset-method! 'foo 'a)))
    (it "fails when no method name is specified"
      (raises? ()
        (%unset-method! (%make-pob #f '() '()))))))



(cond-expand
 ((not protolk-all-tests)
  (test-exit))
 (else))
